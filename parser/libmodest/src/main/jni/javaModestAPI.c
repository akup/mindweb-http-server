#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <modest/finder/finder.h>
#include <myhtml/myhtml.h>
#include <myhtml/serialization.h>
#include <mycss/mycss.h>
#include <mycss/selectors/init.h>
#include <mycss/selectors/serialization.h>

#include "net_aklabs_modest_ModestAPI.h"  /* the JNI-generated glue header file */
#include "sb.h"

#define DIE(msg, java, myself, ...) do { char out_txt[500]; sprintf(out_txt, msg, ##__VA_ARGS__); logToJava(out_txt, java, myself, true); return 0; } while(0)
#define CHECK_STATUS(msg, java, myself, ...) do {if(status) DIE(msg, java, myself, ##__VA_ARGS__);} while(0)

mystatus_t serialization_bad_selectors(const char* buffer, size_t size, void* ctx)
{
  //char out_txt[500];
  //sprintf(out_txt, "%.*s", (int)size, buffer);
  //log(out_txt, )
  printf("%.*s", (int)size, buffer);
  return MyCORE_STATUS_OK;
}

/*
 * This is a private struct allocated for every 'CurlGlue' object.
 */
struct modest_parsers {
  myhtml_t *myhtml;
  mycss_entry_t *css_entry;
  modest_finder_t *finder;

  struct writecallback {
    JNIEnv *java;
    jclass cls; /* global reference */
    jobject object;
  } write;
};

struct html_insertion {
  size_t pos;
  const char* html;
  struct html_insertion* next;
};
typedef struct html_insertion html_insertion_t;
struct myhtml_tree_with_str {
  myhtml_tree_t* html_tree;
  const char* html;

  html_insertion_t* insertions;
  html_insertion_t* last_insertion;
};

typedef struct myhtml_tree_with_str myhtml_tree_with_str_t;

void logToJava(char *msg, JNIEnv *java, jobject myself, bool iserror, ...) {
  jclass clz = (*java)->GetObjectClass(java, myself);
  jmethodID logmsg = (*java)->GetMethodID(java, clz, "logFromC", "(Ljava/lang/String;Z)V");
  jstring jstr = (*java)->NewStringUTF(java, msg);
  jboolean error = JNI_FALSE;
  if (iserror) error = JNI_TRUE;
  (*java)->CallObjectMethod(java, myself, logmsg, jstr, error);
}

JNIEXPORT jlong JNICALL Java_net_aklabs_modest_ModestAPI_jni_1init(JNIEnv *java,
                                                jobject myself)
{
  struct modest_parsers *m_parsers=(struct modest_parsers *)malloc(sizeof(struct modest_parsers));
  if (m_parsers) {
    /* init MyHTML */
    m_parsers->myhtml = myhtml_create();
    mystatus_t status = myhtml_init(m_parsers->myhtml, MyHTML_OPTIONS_DEFAULT, 1, 0);

    CHECK_STATUS("Can't init MyHTML object", java, myself);

    /* create css parser and finder for selectors */
    mycss_t *mycss = mycss_create();
    status = mycss_init(mycss);
    CHECK_STATUS("Can't init MyCSS object", java, myself);

    // currenr entry work init
    m_parsers->css_entry = mycss_entry_create();
    status = mycss_entry_init(mycss, m_parsers->css_entry);

    CHECK_STATUS("Can't init MyCSS Entry object", java, myself);
    m_parsers->finder = modest_finder_create_simple();

    jclass clz = (*java)->GetObjectClass(java, myself);
    jclass cls = (*java)->NewGlobalRef(java, clz);
    jobject object = (*java)->NewGlobalRef(java, myself);
    m_parsers->write.java = java;
    m_parsers->write.cls = cls;
    m_parsers->write.object = object;
  }

  return (uintptr_t)m_parsers; /* nasty typecast */
}

JNIEXPORT void JNICALL Java_net_aklabs_modest_ModestAPI_jni_1cleanup(JNIEnv *java,
                                                jobject myself, jlong jl_modest_parsers) {
  struct modest_parsers *m_parsers = (struct modest_parsers*)(uintptr_t)jl_modest_parsers;

  /* destroy Modest Finder */
  modest_finder_destroy(m_parsers->finder, true);

  /* destroy MyCSS */
  mycss_t *mycss = m_parsers->css_entry->mycss;
  mycss_entry_destroy(m_parsers->css_entry, true);
  mycss_destroy(mycss, true);

  /* destroy MyHTML */
  myhtml_destroy(m_parsers->myhtml);

  free((void *)m_parsers); /* free the struct too */
}


myhtml_tree_with_str_t *parseHtml(const char* html, struct modest_parsers* m_parsers,
                            JNIEnv *java, jobject myself) {
  myhtml_tree_t* html_tree = myhtml_tree_create();
  mystatus_t status = myhtml_tree_init(html_tree, m_parsers->myhtml);

  CHECK_STATUS("Can't init MyHTML Tree object", java, myself);

  status = myhtml_parse(html_tree, MyENCODING_UTF_8, html, strlen(html));
  CHECK_STATUS("Can't parse HTML:\n%s", java, myself, html);

  myhtml_tree_with_str_t* tree_with_str = (myhtml_tree_with_str_t *)malloc(sizeof(myhtml_tree_with_str_t));
  tree_with_str->html_tree = html_tree;
  tree_with_str->html = html;
  tree_with_str->insertions = NULL;
  tree_with_str->last_insertion = NULL;

  return tree_with_str;
}
JNIEXPORT jlong JNICALL Java_net_aklabs_modest_ModestAPI_jni_1parse_1file(JNIEnv *java,
                        jobject myself, jstring j_path, jlong jl_modest_parsers) {
  struct modest_parsers *m_parsers = (struct modest_parsers*)(uintptr_t)jl_modest_parsers;
  const char *file_path = (*java)->GetStringUTFChars(java, j_path, 0);

  FILE *fp = fopen(file_path, "r");

  if( !fp ) return 0;

  fseek(fp, 0L, SEEK_END);
  size_t f_size = ftell(fp);
  fseek(fp, 0L, SEEK_SET);

  char *html = calloc( 1, f_size+1 );
  fread(html, f_size, 1, fp);
  fclose(fp);

/*
  char msg[10];
  sprintf(msg, "%ld", f_size);
  logToJava(msg, java, myself, false);
  logToJava(html, java, myself, false);
*/

  (*java)->ReleaseStringUTFChars(java, j_path, file_path);

  myhtml_tree_with_str_t *tree = parseHtml(html, m_parsers, java, myself);

  return (uintptr_t)tree;
}

JNIEXPORT jlong JNICALL Java_net_aklabs_modest_ModestAPI_jni_1parse(JNIEnv *java,
                        jobject myself, jstring j_html, jlong jl_modest_parsers) {
  struct modest_parsers *m_parsers = (struct modest_parsers*)(uintptr_t)jl_modest_parsers;
  const char *html = (*java)->GetStringUTFChars(java, j_html, 0);

  myhtml_tree_with_str_t* html_tree = parseHtml(html, m_parsers, java, myself);

  //(*java)->ReleaseStringUTFChars(java, j_html, html);

  return (uintptr_t)html_tree; /* nasty typecast */
}

JNIEXPORT void JNICALL Java_net_aklabs_modest_ModestAPI_jni_1cleanup_1tree(JNIEnv *java,
                                                jobject myself, jlong jl_html_tree) {
  myhtml_tree_with_str_t *html_tree = (myhtml_tree_with_str_t*)(uintptr_t)jl_html_tree;

  /* destroy MyHTML Tree */
  myhtml_tree_destroy(html_tree->html_tree);

  //logToJava(html_tree->html, java, myself, false);
  free((void *)html_tree->html);

  //Clean insertions
  if (html_tree->insertions != NULL) {
    logToJava("Should free insertions", java, myself, false);
    html_insertion_t* current;
    html_insertion_t* next = html_tree->insertions;
    do {
      logToJava("Free next insertion!", java, myself, false);
      current = next;
      next = current->next;
      free((void *)current->html);
      free(current);
    } while (next != NULL);
  }

  free((void *)html_tree);
}

JNIEXPORT jlong JNICALL Java_net_aklabs_modest_ModestAPI_jni_1prepare_1selector(JNIEnv *java,
                                                jobject myself, jstring j_selector, jlong jl_modest_parsers) {
  struct modest_parsers *m_parsers = (struct modest_parsers*)(uintptr_t)jl_modest_parsers;
  const char *selector = (*java)->GetStringUTFChars(java, j_selector, 0);

  mystatus_t out_status;
  mycss_selectors_list_t *selectors_list = mycss_selectors_parse(mycss_entry_selectors(m_parsers->css_entry),
                                                       MyENCODING_UTF_8,
                                                       selector, strlen(selector),
                                                       &out_status);
  /* check parsing errors */
  if(selectors_list == NULL || (selectors_list->flags & MyCSS_SELECTORS_FLAGS_SELECTOR_BAD)) {
    //char out_txt[500]; sprintf(out_txt, msg, ##__VA_ARGS__);
    logToJava("Bad CSS Selectors", java, myself, true);

    if(selectors_list) {
      mycss_selectors_serialization_list(mycss_entry_selectors(m_parsers->css_entry), selectors_list,
                                             serialization_bad_selectors, NULL);
      printf("\n");
    }

    return 0;
  }

  (*java)->ReleaseStringUTFChars(java, j_selector, selector);

  return (uintptr_t)selectors_list; /* nasty typecast */
}

JNIEXPORT void JNICALL Java_net_aklabs_modest_ModestAPI_jni_1cleanup_1selector(JNIEnv *java,
                                              jobject myself, jlong jl_selector, jlong jl_modest_parsers) {
  struct modest_parsers *m_parsers = (struct modest_parsers*)(uintptr_t)jl_modest_parsers;
  mycss_selectors_list_t *selectors_list = (mycss_selectors_list_t*)(uintptr_t)jl_selector;

  /* destroy selector list */
  mycss_selectors_list_destroy(mycss_entry_selectors(m_parsers->css_entry), selectors_list, true);
}

myhtml_tree_node_t* getRootNode(myhtml_tree_t *html_tree) {
  myhtml_tree_node_t *node = html_tree->document;
  if (node->token == NULL) {
    //logToJava("not document", java, myself, false);
    node = html_tree->node_html;
  }
  if (node->token == NULL) {
    //logToJava("not html", java, myself, false);
    node = html_tree->node_body;
  }
  if (node->token == NULL) {
    //logToJava("not body", java, myself, false);
    node = node->child;
    while (node && node->token == NULL) {
      //logToJava("next", java, myself, false);
      node = node->next;
    }
  }

  return node;
}

JNIEXPORT jlongArray JNICALL Java_net_aklabs_modest_ModestAPI_jni_1find_1nodes(JNIEnv *java,
        jobject myself, jlong jl_html_tree, jlong jl_css_selector, jlong jl_modest_parsers) {
  struct modest_parsers *m_parsers = (struct modest_parsers*)(uintptr_t)jl_modest_parsers;
  mycss_selectors_list_t *selectors_list = (mycss_selectors_list_t*)(uintptr_t)jl_css_selector;
  myhtml_tree_with_str_t *html_tree = (myhtml_tree_with_str_t*)(uintptr_t)jl_html_tree;

  /* find nodes by selector */
  myhtml_collection_t *collection = NULL;
  modest_finder_by_selectors_list(m_parsers->finder,
                                    getRootNode(html_tree->html_tree),
                                    selectors_list, &collection);

  jlongArray jl=NULL;
  if(collection) {
    jl=(*java)->NewLongArray(java, collection->length);
    //jlong *elements = (*java)->GetLongArrayElements(jl, 0);
    jlong l_arr[collection->length];
    for(size_t i = 0; i < collection->length; i++) {
      l_arr[i] = (uintptr_t)collection->list[i];
    }
    (*java)->SetLongArrayRegion(java, jl, 0, collection->length, (jlong*)l_arr);
  }

  /* destroy all */
  myhtml_collection_destroy(collection);

  return jl;
}

JNIEXPORT jstring JNICALL Java_net_aklabs_modest_ModestAPI_jni_1node_1name(JNIEnv *java,
                    jobject myself, jlong jl_node) {
  myhtml_tree_node_t *node = (myhtml_tree_node_t*)(uintptr_t)jl_node;

  size_t length;
  const char *tag = myhtml_tag_name_by_id(node->tree, node->tag_id, &length);
  return (*java)->NewStringUTF(java, tag);
}

/*
 * Class:     net_aklabs_modest_ModestAPI
 * Method:    jni_attributes
 * Signature: (J)[Lnet/aklabs/modest/HtmlAttribute;
 */
JNIEXPORT jobjectArray JNICALL Java_net_aklabs_modest_ModestAPI_jni_1attributes(JNIEnv *java,
                    jobject myself, jlong jl_node) {
  myhtml_tree_node_t *node = (myhtml_tree_node_t*)(uintptr_t)jl_node;
  myhtml_tree_attr_t *attr = node->token->attr_first;

  jclass attrJClass = (*java)->FindClass(java, "net/aklabs/modest/HtmlAttribute");
  jmethodID attrJConstructor1 = (*java)->GetMethodID(java, attrJClass, "<init>", "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V");
  jmethodID attrJConstructor2 = (*java)->GetMethodID(java, attrJClass, "<init>", "(Ljava/lang/String;Ljava/lang/String;)V");

  int i = 0;
  while(attr) {
    i += 1;
    attr = attr->next;
  }
  attr = node->token->attr_first;

  jobjectArray ret = (jobjectArray) (*java)->NewObjectArray(java, i, attrJClass, NULL);

  i = 0;
  while(attr) {

    jstring namesp = NULL;
    switch (attr->ns) {
      case MyHTML_NAMESPACE_XML:
        namesp = (*java)->NewStringUTF(java, "xml");
        break;
      case MyHTML_NAMESPACE_XMLNS:
        namesp = (*java)->NewStringUTF(java, "xmlns");
        break;
      case MyHTML_NAMESPACE_XLINK:
        namesp = (*java)->NewStringUTF(java, "xlink");
        break;
      default:
        break;
    }

    jstring key = NULL;
    jstring value = NULL;
    size_t length;
    const char *data = myhtml_attribute_key(attr, &length);
    if (data) {
      key = (*java)->NewStringUTF(java, data);
    } else {
      key = (*java)->NewStringUTF(java, "");
    }

    data = myhtml_attribute_value(attr, &length);
    if (data) {
      value = (*java)->NewStringUTF(java, data);
    } else {
      value = (*java)->NewStringUTF(java, "");
    }

    jobject attrObj;
    if (namesp != NULL) {
        attrObj = (*java)->NewObject(java, attrJClass, attrJConstructor1, key, value, namesp);
    } else {
        attrObj = (*java)->NewObject(java, attrJClass, attrJConstructor2, key, value);
    }

    (*java)->SetObjectArrayElement(java, ret, i, attrObj);

    i += 1;
    attr = attr->next;
  }
  return ret;
}

JNIEXPORT jobject JNICALL Java_net_aklabs_modest_ModestAPI_jni_1attribute(JNIEnv *java,
                    jobject myself, jlong jl_node, jstring j_attribute) {
  myhtml_tree_node_t *node = (myhtml_tree_node_t*)(uintptr_t)jl_node;
  myhtml_tree_attr_t *attr = node->token->attr_first;

  jclass attrJClass = (*java)->FindClass(java, "net/aklabs/modest/HtmlAttribute");
  jmethodID attrJConstructor1 = (*java)->GetMethodID(java, attrJClass, "<init>", "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V");
  jmethodID attrJConstructor2 = (*java)->GetMethodID(java, attrJClass, "<init>", "(Ljava/lang/String;Ljava/lang/String;)V");

  while(attr) {
    jstring namesp = NULL;
    switch (attr->ns) {
      case MyHTML_NAMESPACE_XML:
        namesp = (*java)->NewStringUTF(java, "xml");
        break;
      case MyHTML_NAMESPACE_XMLNS:
        namesp = (*java)->NewStringUTF(java, "xmlns");
        break;
      case MyHTML_NAMESPACE_XLINK:
        namesp = (*java)->NewStringUTF(java, "xlink");
        break;
      default:
        break;
    }

    jstring key = NULL;
    jstring value = NULL;
    size_t length;
    const char *data = myhtml_attribute_key(attr, &length);
    if (data) {
      key = (*java)->NewStringUTF(java, data);
    } else {
      key = (*java)->NewStringUTF(java, "");
    }

    data = myhtml_attribute_value(attr, &length);
    if (data) {
      value = (*java)->NewStringUTF(java, data);
    } else {
      value = (*java)->NewStringUTF(java, "");
    }

    if (namesp != NULL) {
      return (*java)->NewObject(java, attrJClass, attrJConstructor1, key, value, namesp);
    } else {
      return (*java)->NewObject(java, attrJClass, attrJConstructor2, key, value);
    }

    attr = attr->next;
  }

  return NULL;
}


struct string {
  myhtml_t *myhtml;
  mycss_entry_t *css_entry;
  modest_finder_t *finder;
};
mystatus_t serialization_callback(const char* buffer, size_t size, void* ptr)
{
  StringBuilder *sb = (StringBuilder*)ptr;
  sb_append(sb, buffer);
  return MyCORE_STATUS_OK;
}
JNIEXPORT jstring JNICALL Java_net_aklabs_modest_ModestAPI_jni_1print_1node(JNIEnv *java,
                    jobject myself, jlong jl_node) {
  myhtml_tree_node_t *node = (myhtml_tree_node_t*)(uintptr_t)jl_node;

  // create an empty string builder
  StringBuilder *sb = sb_create();
  myhtml_serialization_node_callback(node, serialization_callback, sb);

  char *str = sb_concat(sb);
  return (*java)->NewStringUTF(java, str);
}

JNIEXPORT jstring JNICALL Java_net_aklabs_modest_ModestAPI_jni_1print_1node_1tree(JNIEnv *java,
                    jobject myself, jlong jl_node) {
  myhtml_tree_node_t *node = (myhtml_tree_node_t*)(uintptr_t)jl_node;

  // create an empty string builder
  StringBuilder *sb = sb_create();
  myhtml_serialization_tree_callback(node, serialization_callback, sb);

  char *str = sb_concat(sb);
  jstring ret = (*java)->NewStringUTF(java, str);
  sb_free(sb);
  return ret;
}

JNIEXPORT jstring JNICALL Java_net_aklabs_modest_ModestAPI_jni_1print_1tree(JNIEnv *java,
                    jobject myself, jlong jl_html_tree) {
  myhtml_tree_with_str_t *html_tree = (myhtml_tree_with_str_t*)(uintptr_t)jl_html_tree;

  myhtml_tree_node_t *node = getRootNode(html_tree->html_tree);

  // create an empty string builder
  StringBuilder *sb = sb_create();
  while (node) {
    myhtml_serialization_tree_callback(node, serialization_callback, sb);
    node = node->next;

    /*
    myhtml_token_node_t *token_node = node->token;
      if (token_node) {
        char msg[100];
          sprintf(msg, "%ld - %ld", token_node->element_begin, token_node->element_length);
          logToJava(msg, java, myself, false);
      }
      */
  }

  char *str = sb_concat(sb);
  jstring ret = (*java)->NewStringUTF(java, str);
  sb_free(sb);
  return ret;
}

JNIEXPORT jboolean JNICALL Java_net_aklabs_modest_ModestAPI_jni_1add_1row_1html(JNIEnv *java,
                    jobject myself, jlong jl_html_tree, jlong jl_node, jstring j_html) {
  myhtml_tree_with_str_t *html_tree = (myhtml_tree_with_str_t*)(uintptr_t)jl_html_tree;
  myhtml_tree_node_t *node = (myhtml_tree_node_t*)(uintptr_t)jl_node;

  myhtml_token_node_t *token_node = node->token;
  if (token_node != NULL) {
    char out_txt[500];
    sprintf(out_txt, "%ld - %ld; %ld - %ld",
        token_node->raw_begin,
        token_node->raw_length,
        token_node->element_begin,
        token_node->element_length);
    logToJava(out_txt, java, myself, false);

    html_insertion_t *insertion=(html_insertion_t*)malloc(sizeof(html_insertion_t));
    insertion->pos = token_node->element_begin + token_node->element_length;
    insertion->html = (*java)->GetStringUTFChars(java, j_html, 0);
    insertion->next = NULL;

    if (html_tree->last_insertion == NULL) {
      html_tree->insertions = insertion;
      html_tree->last_insertion = insertion;
    } else {
      html_tree->last_insertion->next = insertion;
      html_tree->last_insertion = insertion;
    }

    //myhtml_tree_node_t *insertion_node = getRootNode(tree->html_tree);
    //myhtml_tree_node_add_child(node, insertion_node);

    return JNI_TRUE;
  } else {
    return JNI_FALSE;
  }
}

int insertionsCmpFunc(const void * a, const void * b) {
  return ( (*(html_insertion_t**)a)->pos - (*(html_insertion_t**)b)->pos );
}
JNIEXPORT jstring JNICALL Java_net_aklabs_modest_ModestAPI_jni_1print_1row_1with_1insertions(JNIEnv *java,
                        jobject myself, jlong jl_html_tree) {
  myhtml_tree_with_str_t *html_tree = (myhtml_tree_with_str_t*)(uintptr_t)jl_html_tree;

  html_insertion_t* next_insertion = html_tree->insertions;
  int count = 0;
  while (next_insertion != NULL) {
    count += 1;
    next_insertion = next_insertion->next;
  }
  if (count > 0) {
    html_insertion_t* insertions_arr[count];
    next_insertion = html_tree->insertions;
    count = 0;
    while (next_insertion != NULL) {
      insertions_arr[count] = next_insertion;
      count += 1;
      next_insertion = next_insertion->next;
    }


    qsort(insertions_arr, count, sizeof(html_insertion_t*), insertionsCmpFunc);

    size_t pos = 0;
    StringBuilder *sb = sb_create();
    for (int i=0; i<count; i++) {
      size_t length = insertions_arr[i]->pos - pos;

      char subbuff[length+1];
      memcpy( subbuff, &(html_tree->html[pos]), length );
      subbuff[length] = '\0';
      sb_append(sb, subbuff);

      sb_append(sb, insertions_arr[i]->html);

      pos = insertions_arr[i]->pos;
    }

    size_t str_len = strlen(html_tree->html);
    if (pos < str_len) {
      char subbuff[str_len+1];
      memcpy( subbuff, &(html_tree->html[pos]), str_len+1 );

      sb_append(sb, subbuff);
    }

    char *str = sb_concat(sb);
    jstring ret = (*java)->NewStringUTF(java, str);
    sb_free(sb);
    return ret;
  } else {
    return (*java)->NewStringUTF(java, html_tree->html);
  }
}