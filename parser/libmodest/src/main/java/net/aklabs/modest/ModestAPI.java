package net.aklabs.modest;/*
 * The curl class is a JNI wrapper for libcurl. Please bear with me, I'm no
 * true java dude (yet). Improve what you think is bad and send me the updates!
 * daniel.se
 *
 * This is meant as a raw, crude and low-level interface to libcurl. If you
 * want fancy stuff, build upon this.
 */

import org.pmw.tinylog.Logger;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.concurrent.locks.ReentrantLock;

public class ModestAPI {
  private static boolean loaded = false;
  
  private static java.lang.reflect.Field LIBRARIES;
  static {
    try {
      LIBRARIES = ClassLoader.class.getDeclaredField("loadedLibraryNames");
      LIBRARIES.setAccessible(true);
    } catch (NoSuchFieldException e) {
      Logger.error(e);
      //e.printStackTrace();
    }
  }
  public static String[] getLoadedLibraries(final ClassLoader loader) throws IllegalAccessException {
    if (LIBRARIES.get(loader) instanceof java.util.HashSet) {
      final java.util.HashSet<String> libraries = (java.util.HashSet<String>) LIBRARIES.get(loader);
      return libraries.toArray(new String[] {});
    } else if (LIBRARIES.get(loader) instanceof java.util.Vector) {
      final Vector<String> libraries = (Vector<String>) LIBRARIES.get(loader);
      return libraries.toArray(new String[] {});
    }

    return new String[] {};
  }

  static {
    if (!loaded)
      try {
        // Loading up libjavacurl.so
        Logger.debug("Loaded libraries");
        boolean loadedBefore = false;
        for (String lib: getLoadedLibraries(ClassLoader.getSystemClassLoader())) {
          int lastSlash = lib.lastIndexOf("/");
          if (lastSlash > -1)
            lib = lib.substring(lastSlash + 1);
          int extention = lib.lastIndexOf(".");
          if (extention > -1)
            lib = lib.substring(0, extention);
          Logger.debug("Lib: " + lib);

          if (lib.equals("libmodest")) {
            loadedBefore = true;
            break;
          }
        }

        if (!loadedBefore) {
          Logger.debug(System.getProperty("java.library.path"));
          //System.loadLibrary("modest");
          System.loadLibrary("javamodestapi");
        }

        loaded=true;
      } catch (Exception e) {
        e.printStackTrace();
      }
  }

  public ModestAPI() {
      try {
        modest_handle = jni_init();
        System.out.println("modest jni_init "+modest_handle);
      } catch (Exception e) {
        e.printStackTrace();
      }
  }
  
  private void logFromC(String msg, boolean error) {
    if (msg == null) if (error) Logger.error("NULL"); else Logger.debug("NULL");
    else {
      if (error) Logger.error("ERROR FROM C: " + msg);
      else Logger.debug("LOG FROM C: " + msg);
    }
  }

  /*
  public void init(){
  	//System.out.println("INIT CURL");
    //int res = setopt(CURL.OPT_WRITEFUNCTION, this);
    //if ("true".equalsIgnoreCase(System.getProperty("curl.verbose", "false")))
    //  setopt(CURL.OPT_VERBOSE, 1);
    //System.out.println("INIT CURL OK: CURLOPT_WRITEFUNCTION "+res);
  }
   */

  public void finalize() {
    jni_cleanup(modest_handle);
  }

  private long modest_handle;

  public long parseFile(String path) {
    return jni_parse_file(path, modest_handle);
  }
  public long parse(String html) {
    return jni_parse(html, modest_handle);
  }
  public void cleanup_tree(long html_tree) {
    jni_cleanup_tree(html_tree);
  }

  public void selectNodes(String html, String css_selector) {
    long html_tree = jni_parse(html, modest_handle);

    selectNodes(html_tree, css_selector);

    jni_cleanup_tree(html_tree);
  }
  public long[] selectNodes(long html_tree, String css_selector) {
    long jl_css_selector = jni_prepare_selector(css_selector, modest_handle);

    long[] nodes = jni_find_nodes(html_tree, jl_css_selector, modest_handle);

    jni_cleanup_selector(jl_css_selector, modest_handle);

    return nodes;
  }
  public boolean addRowHtml(long html_tree, long html_node, String text) {
    return jni_add_row_html(html_tree, html_node, text);
  }

  /* constructor and destructor for the libcurl handle */
  private native long jni_init();
  private native void jni_cleanup(long modest_handle);

  private native long jni_parse_file(String path, long modest_handle);
  private native long jni_parse(String html, long modest_handle);
  private native void jni_cleanup_tree(long html_tree);

  private native long jni_prepare_selector(String css_selector, long modest_handle);
  private native void jni_cleanup_selector(long css_selector, long modest_handle);

  private native long[] jni_find_nodes(long html_tree, long css_selector, long modest_handle);

  native String jni_node_name(long html_node);
  native HtmlAttribute[] jni_attributes(long html_node);
  native HtmlAttribute jni_attribute(long html_node, String name);
  private native String jni_print_node(long html_node);
  native String jni_print_node_tree(long html_node);

  native String jni_print_tree(long html_tree);

  native String jni_print_row_with_insertions(long html_tree);

  private native boolean jni_add_row_html(long html_tree, long html_node, String text);
}

