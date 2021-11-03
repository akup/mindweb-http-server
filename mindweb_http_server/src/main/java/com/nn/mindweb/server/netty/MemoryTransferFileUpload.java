package com.nn.mindweb.server.netty;

import io.netty.buffer.ByteBuf;
import io.netty.channel.ChannelException;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpHeaderValues;
import io.netty.handler.codec.http.multipart.FileUpload;
import io.netty.handler.codec.http.multipart.InterfaceHttpData;
import io.netty.util.internal.ObjectUtil;

import java.io.IOException;
import java.nio.charset.Charset;

/**
 * Default FileUpload implementation that stores file into memory.<br><br>
 *
 * Warning: be aware of the memory limitation.
 */
public class MemoryTransferFileUpload extends AbstractMemoryTransferHttpData implements FileUpload {

    private String filename;

    private String contentType;

    private String contentTransferEncoding;

    public MemoryTransferFileUpload(String name, String filename, String contentType,
                                    String contentTransferEncoding, Charset charset, long size) {
        super(name, charset, size);
        setFilename(filename);
        setContentType(contentType);
        setContentTransferEncoding(contentTransferEncoding);
    }

    @Override
    public InterfaceHttpData.HttpDataType getHttpDataType() {
        return InterfaceHttpData.HttpDataType.FileUpload;
    }

    @Override
    public String getFilename() {
        return filename;
    }

    @Override
    public void setFilename(String filename) {
        this.filename = ObjectUtil.checkNotNull(filename, "filename");
    }

    @Override
    public int hashCode() {
        return FileUploadUtil.hashCode(this);
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof FileUpload && FileUploadUtil.equals(this, (FileUpload) o);
    }

    @Override
    public int compareTo(InterfaceHttpData o) {
        if (!(o instanceof FileUpload)) {
            throw new ClassCastException("Cannot compare " + getHttpDataType() +
                    " with " + o.getHttpDataType());
        }
        return compareTo((FileUpload) o);
    }

    public int compareTo(FileUpload o) {
        return FileUploadUtil.compareTo(this, o);
    }

    @Override
    public void setContentType(String contentType) {
        this.contentType = ObjectUtil.checkNotNull(contentType, "contentType");
    }

    @Override
    public String getContentType() {
        return contentType;
    }

    @Override
    public String getContentTransferEncoding() {
        return contentTransferEncoding;
    }

    @Override
    public void setContentTransferEncoding(String contentTransferEncoding) {
        this.contentTransferEncoding = contentTransferEncoding;
    }

    @Override
    public String toString() {
        return HttpHeaderNames.CONTENT_DISPOSITION + ": " +
                HttpHeaderValues.FORM_DATA + "; " + HttpHeaderValues.NAME + "=\"" + getName() +
                "\"; " + HttpHeaderValues.FILENAME + "=\"" + filename + "\"\r\n" +
                HttpHeaderNames.CONTENT_TYPE + ": " + contentType +
                (getCharset() != null? "; " + HttpHeaderValues.CHARSET + '=' + getCharset().name() + "\r\n" : "\r\n") +
                HttpHeaderNames.CONTENT_LENGTH + ": " + length() + "\r\n" +
                "Completed: " + isCompleted() +
                "\r\nIsInMemory: " + isInMemory();
    }

    @Override
    public FileUpload copy() {
        final ByteBuf content = content();
        return replace(content != null ? content.copy() : content);
    }

    @Override
    public FileUpload duplicate() {
        final ByteBuf content = content();
        return replace(content != null ? content.duplicate() : content);
    }

    @Override
    public FileUpload retainedDuplicate() {
        ByteBuf content = content();
        if (content != null) {
            content = content.retainedDuplicate();
            boolean success = false;
            try {
                FileUpload duplicate = replace(content);
                success = true;
                return duplicate;
            } finally {
                if (!success) {
                    content.release();
                }
            }
        } else {
            return replace(null);
        }
    }

    @Override
    public FileUpload replace(ByteBuf content) {
        io.netty.handler.codec.http.multipart.MemoryFileUpload upload = new io.netty.handler.codec.http.multipart.MemoryFileUpload(
                getName(), getFilename(), getContentType(), getContentTransferEncoding(), getCharset(), size);
        if (content != null) {
            try {
                upload.setContent(content);
                return upload;
            } catch (IOException e) {
                throw new ChannelException(e);
            }
        }
        return upload;
    }

    @Override
    public FileUpload retain() {
        super.retain();
        return this;
    }

    @Override
    public FileUpload retain(int increment) {
        super.retain(increment);
        return this;
    }

    @Override
    public FileUpload touch() {
        super.touch();
        return this;
    }

    @Override
    public FileUpload touch(Object hint) {
        super.touch(hint);
        return this;
    }
}
