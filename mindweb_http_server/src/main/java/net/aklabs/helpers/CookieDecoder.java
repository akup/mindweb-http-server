package net.aklabs.helpers;


import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class CookieDecoder {
    public static Map.Entry<List<String>, List<String>> extractKeyValuePairs(final String header) {
        List<String> names = new ArrayList<String>(8);
        List<String> values = new ArrayList<String>(8);

        final int headerLen  = header.length();
        loop: for (int i = 0;;) {

            // Skip spaces and separators.
            for (;;) {
                if (i == headerLen) {
                    break loop;
                }
                switch (header.charAt(i)) {
                    case '\t': case '\n': case 0x0b: case '\f': case '\r':
                    case ' ':  case ',':  case ';':
                        i ++;
                        continue;
                }
                break;
            }

            // Skip '$'.
            for (;;) {
                if (i == headerLen) {
                    break loop;
                }
                if (header.charAt(i) == '$') {
                    i ++;
                    continue;
                }
                break;
            }

            String name;
            String value;

            if (i == headerLen) {
                name = null;
                value = null;
            } else {
                int newNameStart = i;
                keyValLoop: for (;;) {
                    switch (header.charAt(i)) {
                        case ';':
                            // NAME; (no value till ';')
                            name = header.substring(newNameStart, i);
                            value = null;
                            break keyValLoop;
                        case '=':
                            // NAME=VALUE
                            name = header.substring(newNameStart, i);
                            i ++;
                            if (i == headerLen) {
                                // NAME= (empty value, i.e. nothing after '=')
                                value = "";
                                break keyValLoop;
                            }

                            int newValueStart = i;
                            char c = header.charAt(i);
                            if (c == '"' || c == '\'') {
                                // NAME="VALUE" or NAME='VALUE'
                                StringBuilder newValueBuf = new StringBuilder(header.length() - i);
                                final char q = c;
                                boolean hadBackslash = false;
                                i ++;
                                for (;;) {
                                    if (i == headerLen) {
                                        value = newValueBuf.toString();
                                        break keyValLoop;
                                    }
                                    if (hadBackslash) {
                                        hadBackslash = false;
                                        c = header.charAt(i ++);
                                        switch (c) {
                                            case '\\': case '"': case '\'':
                                                // Escape last backslash.
                                                newValueBuf.setCharAt(newValueBuf.length() - 1, c);
                                                break;
                                            default:
                                                // Do not escape last backslash.
                                                newValueBuf.append(c);
                                        }
                                    } else {
                                        c = header.charAt(i ++);
                                        if (c == q) {
                                            value = newValueBuf.toString();
                                            break keyValLoop;
                                        }
                                        newValueBuf.append(c);
                                        if (c == '\\') {
                                            hadBackslash = true;
                                        }
                                    }
                                }
                            } else {
                                // NAME=VALUE;
                                int semiPos = header.indexOf(';', i);
                                if (semiPos > 0) {
                                    value = header.substring(newValueStart, semiPos);
                                    i = semiPos;
                                } else {
                                    value = header.substring(newValueStart);
                                    i = headerLen;
                                }
                            }
                            break keyValLoop;
                        default:
                            i ++;
                    }

                    if (i == headerLen) {
                        // NAME (no value till the end of string)
                        name = header.substring(newNameStart);
                        value = null;
                        break;
                    }
                }
            }

            names.add(name);
            values.add(value);
        }
        return new AbstractMap.SimpleEntry<List<String>, List<String>>(names, values);
    }
}
