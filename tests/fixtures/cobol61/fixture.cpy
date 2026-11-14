      * @file fixture.cpy
      * @brief COBOL-61 golden fixture with REDEFINES, SYNC, JUSTIFIED.
      * @record INV-REC Inventory record for COBOL-61.
       01  INV-REC.
      * @field SKU Item stock code.
           05  SKU                   PICTURE X(10) JUSTIFIED.
      * @field SKU-PFX Redefined prefix over SKU.
      * @cdd-keep
           05  SKU-PFX REDEFINES SKU PICTURE X(10).
      * @field QTY Quantity on hand.
           05  QTY                   PICTURE 9(9) SYNCHRONIZED.
