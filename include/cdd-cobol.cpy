      * @file cdd-cobol.cpy
      * @brief Public SDK header copybook for cdd-cobol consumers.
      * @record CDD-SDK-VERSION-HDR SDK version identification banner.
       01  CDD-SDK-VERSION-HDR.
      * @field CDD-VER-TAG Magic type-safety tag ('CDDVER01').
           05  CDD-VER-TAG               PIC X(8) VALUE "CDDVER01".
      * @field CDD-VER-TAG-VALID Condition confirming valid VER tag.
               88  CDD-VER-TAG-VALID              VALUE "CDDVER01".
      * @field CDD-VER-STRING Semantic SDK version string.
           05  CDD-VER-STRING            PIC X(16) VALUE "1.0.0".
       COPY "cdd-opts.cpy".
       COPY "cdd-types.cpy".
       COPY "cdd-err.cpy".
