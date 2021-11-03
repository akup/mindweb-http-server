package com.nn.mindweb.server.netty;

import io.netty.buffer.ByteBuf;
import io.netty.handler.codec.http.HttpConstants;
import io.netty.handler.codec.http.multipart.AbstractHttpData;
import io.netty.handler.codec.http.multipart.HttpData;
import io.netty.util.internal.ObjectUtil;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import static io.netty.buffer.Unpooled.*;

/**
 * Abstract Memory HttpData implementation
 */
public abstract class AbstractMemoryTransferHttpData extends AbstractHttpData {

    private List<ByteBuf> byteBufs;
    private int chunkPosition;

    protected AbstractMemoryTransferHttpData(String name, Charset charset, long size) {
        super(name, charset, size);
    }

    @Override
    public void setContent(ByteBuf buffer) throws IOException {
        ObjectUtil.checkNotNull(buffer, "buffer");
        long localsize = buffer.readableBytes();
        checkSize(localsize);
        if (definedSize > 0 && definedSize < localsize) {
            throw new IOException("Out of size: " + localsize + " > " +
                    definedSize);
        }
        if (byteBufs != null) {
            Iterator<ByteBuf> it = byteBufs.iterator();
            while (it.hasNext()) {
                it.next().release();
            }
        }
        byteBufs = new java.util.concurrent.CopyOnWriteArrayList<>();
        byteBufs.add(buffer);
        size = localsize;
        setCompleted();
    }

    @Override
    public void setContent(InputStream inputStream) throws IOException {
        ObjectUtil.checkNotNull(inputStream, "inputStream");
        byte[] bytes = new byte[4096 * 4];
        ByteBuf buffer = buffer();
        int written = 0;
        try {
            int read = inputStream.read(bytes);
            while (read > 0) {
                buffer.writeBytes(bytes, 0, read);
                written += read;
                checkSize(written);
                read = inputStream.read(bytes);
            }
        } catch (IOException e) {
            buffer.release();
            throw e;
        }
        size = written;
        if (definedSize > 0 && definedSize < size) {
            buffer.release();
            throw new IOException("Out of size: " + size + " > " + definedSize);
        }
        if (byteBufs != null) {
            Iterator<ByteBuf> it = byteBufs.iterator();
            while (it.hasNext()) {
                it.next().release();
            }
        }
        byteBufs = new java.util.concurrent.CopyOnWriteArrayList<>();
        byteBufs.add(buffer.copy());
        setCompleted();
    }

    @Override
    public void addContent(ByteBuf buffer, boolean last)
            throws IOException {
        if (buffer != null) {
            long localsize = buffer.readableBytes();
            checkSize(size + localsize);
            if (definedSize > 0 && definedSize < size + localsize) {
                throw new IOException("Out of size: " + (size + localsize) +
                        " > " + definedSize);
            }
            size += localsize;
            if (byteBufs == null) {
                byteBufs = new java.util.concurrent.CopyOnWriteArrayList<>();
                byteBufs.add(buffer.copy());
            } else {
                byteBufs.add(buffer.copy());
            }
            /*
            else if (byteBuf instanceof CompositeByteBuf) {
                CompositeByteBuf cbb = (CompositeByteBuf) byteBuf;
                cbb.addComponent(true, buffer);
            } else {
                CompositeByteBuf cbb = compositeBuffer(Integer.MAX_VALUE);
                cbb.addComponents(true, byteBuf, buffer);
                byteBuf = cbb;
            }
             */
        }
        if (last) {
            setCompleted();
        } else {
            ObjectUtil.checkNotNull(buffer, "buffer");
        }
    }

    @Override
    public void setContent(File file) throws IOException {
        ObjectUtil.checkNotNull(file, "file");

        long newsize = file.length();
        if (newsize > Integer.MAX_VALUE) {
            throw new IllegalArgumentException("File too big to be loaded in memory");
        }
        checkSize(newsize);
        RandomAccessFile accessFile = new RandomAccessFile(file, "r");
        ByteBuffer byteBuffer;
        try {
            try (FileChannel fileChannel = accessFile.getChannel()) {
                byte[] array = new byte[(int) newsize];
                byteBuffer = ByteBuffer.wrap(array);
                int read = 0;
                while (read < newsize) {
                    read += fileChannel.read(byteBuffer);
                }
            }
        } finally {
            accessFile.close();
        }
        byteBuffer.flip();
        if (byteBufs != null) {
            Iterator<ByteBuf> it = byteBufs.iterator();
            while (it.hasNext()) {
                it.next().touch().release();
            }
        }
        byteBufs = new java.util.concurrent.CopyOnWriteArrayList<>();
        byteBufs.add(wrappedBuffer(Integer.MAX_VALUE, byteBuffer));
        size = newsize;
        setCompleted();
    }

    @Override
    public void delete() {
        if (byteBufs != null) {
            Iterator<ByteBuf> it = byteBufs.iterator();
            while (it.hasNext()) {
                it.next().release();
            }
            byteBufs = null;
        }
    }

    @Override
    public byte[] get() {
        System.out.println("Get bytes");
        if (byteBufs == null) {
            return EMPTY_BUFFER.array();
        }
        Iterator<ByteBuf> it = byteBufs.iterator();
        int fullSize = 0;
        List<byte[]> bytes = new ArrayList<>();
        while (it.hasNext()) {
            ByteBuf byteBuf = it.next();
            int size = byteBuf.readableBytes();
            fullSize += size;
            byte[] array = new byte[size];
            //byteBuf.getBytes(byteBuf.readerIndex(), array);
            byteBuf.readBytes(array);
            byteBuf.discardReadBytes();
            byteBuf.release();
            bytes.add(array);
        }
        byteBufs.clear();

        byte[] array = new byte[fullSize];
        int copyPos = 0;
        Iterator<byte[]> itB = bytes.iterator();
        while (itB.hasNext()) {
            byte[] from = itB.next();
            System.arraycopy(from, 0, array, copyPos, from.length);
            copyPos += from.length;
        }
        //byteBuf.getBytes(byteBuf.readerIndex(), array);
        return array;
    }

    @Override
    public String getString() {
        return getString(HttpConstants.DEFAULT_CHARSET);
    }

    @Override
    public String getString(Charset encoding) {
        return "";
        /*
        if (byteBuf == null) {
            return "";
        }
        if (encoding == null) {
            encoding = HttpConstants.DEFAULT_CHARSET;
        }
        return byteBuf.toString(encoding);
         */
    }

    /**
     * Utility to go from a In Memory FileUpload
     * to a Disk (or another implementation) FileUpload
     * @return the attached ByteBuf containing the actual bytes
     */
    @Override
    public ByteBuf getByteBuf() {
        return EMPTY_BUFFER;
        //return byteBuf;
    }

    @Override
    public ByteBuf getChunk(int length) throws IOException {
        throw new IOException("Not implemented");
        /*
        if (byteBuf == null || length == 0 || byteBuf.readableBytes() == 0) {
            chunkPosition = 0;
            return EMPTY_BUFFER;
        }
        int sizeLeft = byteBuf.readableBytes() - chunkPosition;
        if (sizeLeft == 0) {
            chunkPosition = 0;
            return EMPTY_BUFFER;
        }
        int sliceLength = length;
        if (sizeLeft < length) {
            sliceLength = sizeLeft;
        }
        ByteBuf chunk = byteBuf.retainedSlice(chunkPosition, sliceLength);
        chunkPosition += sliceLength;
        return chunk;
         */
    }

    @Override
    public boolean isInMemory() {
        return true;
    }

    @Override
    public boolean renameTo(File dest) throws IOException {
        throw new IOException("Not implemented");
        /*
        ObjectUtil.checkNotNull(dest, "dest");
        if (byteBuf == null) {
            // empty file
            if (!dest.createNewFile()) {
                throw new IOException("file exists already: " + dest);
            }
            return true;
        }
        int length = byteBuf.readableBytes();
        int written = 0;
        try (RandomAccessFile accessFile = new RandomAccessFile(dest, "rw")) {
            try (FileChannel fileChannel = accessFile.getChannel()) {
                if (byteBuf.nioBufferCount() == 1) {
                    ByteBuffer byteBuffer = byteBuf.nioBuffer();
                    while (written < length) {
                        written += fileChannel.write(byteBuffer);
                    }
                } else {
                    ByteBuffer[] byteBuffers = byteBuf.nioBuffers();
                    while (written < length) {
                        written += fileChannel.write(byteBuffers);
                    }
                }
                fileChannel.force(false);
            }
        }
        return written == length;
         */
    }

    @Override
    public File getFile() throws IOException {
        throw new IOException("Not represented by a file");
    }

    @Override
    public HttpData touch() {
        return touch(null);
    }

    @Override
    public HttpData touch(Object hint) {
        /*
        if (byteBuf != null) {
            byteBuf.touch(hint);
        }
         */
        return this;
    }
}
