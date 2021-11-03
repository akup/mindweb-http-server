package com.nn.mindweb.server.messages;

import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

final class HttpHeaderDateFormat extends SimpleDateFormat {
  private static final long serialVersionUID = -925286159755905325L;

  private final SimpleDateFormat format1 = new HttpHeaderDateFormatObsolete1();
  private final SimpleDateFormat format2 = new HttpHeaderDateFormatObsolete2();

  private static final ThreadLocal<HttpHeaderDateFormat> FORMAT_THREAD_LOCAL =
          new ThreadLocal<HttpHeaderDateFormat>() {
      @Override
      protected HttpHeaderDateFormat initialValue() {
          return new HttpHeaderDateFormat();
      }
  };

  public static HttpHeaderDateFormat get() {
      return FORMAT_THREAD_LOCAL.get();
  }

  /**
   * Standard date format<p>
   * Sun, 06 Nov 1994 08:49:37 GMT -> E, d MMM yyyy HH:mm:ss z
   */
  private HttpHeaderDateFormat() {
      super("E, dd MMM yyyy HH:mm:ss z", Locale.ENGLISH);
      setTimeZone(TimeZone.getTimeZone("GMT"));
  }

  @Override
  public Date parse(String text, ParsePosition pos) {
      Date date = super.parse(text, pos);
      if (date == null) {
          date = format1.parse(text, pos);
      }
      if (date == null) {
          date = format2.parse(text, pos);
      }
      return date;
  }

  /**
   * First obsolete format<p>
   * Sunday, 06-Nov-94 08:49:37 GMT -> E, d-MMM-y HH:mm:ss z
   */
  private static final class HttpHeaderDateFormatObsolete1 extends SimpleDateFormat {
      private static final long serialVersionUID = -3178072504225114298L;

      HttpHeaderDateFormatObsolete1() {
          super("E, dd-MMM-yy HH:mm:ss z", Locale.ENGLISH);
          setTimeZone(TimeZone.getTimeZone("GMT"));
      }
  }

  /**
   * Second obsolete format
   * <p>
   * Sun Nov 6 08:49:37 1994 -> EEE, MMM d HH:mm:ss yyyy
   */
  private static final class HttpHeaderDateFormatObsolete2 extends SimpleDateFormat {
      private static final long serialVersionUID = 3010674519968303714L;

      HttpHeaderDateFormatObsolete2() {
          super("E MMM d HH:mm:ss yyyy", Locale.ENGLISH);
          setTimeZone(TimeZone.getTimeZone("GMT"));
      }
  }
}