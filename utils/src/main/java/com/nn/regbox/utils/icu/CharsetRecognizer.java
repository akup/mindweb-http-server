package com.nn.regbox.utils.icu;

abstract class CharsetRecognizer {
  /**
   * Get the IANA name of this charset.
   * @return the charset name.
   */
  abstract String      getName();
  
  /**
   * Get the ISO language code for this charset.
   * @return the language code, or <code>null</code> if the language cannot be determined.
   */
  public   String      getLanguage()
  {
      return null;
  }
  
  /**
   * Test the match of this charset with the input text data
   *      which is obtained via the CharsetDetector object.
   * 
   * @param det  The CharsetDetector, which contains the input text
   *             to be checked for being in this charset.
   * @return     A CharsetMatch object containing details of match
   *             with this charset, or null if there was no match.
   */
  abstract CharsetMatch  match(CharsetDetector det);

}
