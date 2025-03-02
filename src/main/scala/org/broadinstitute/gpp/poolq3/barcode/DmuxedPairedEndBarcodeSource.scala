/*
 * Copyright (c) 2024 The Broad Institute, Inc. All rights reserved.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package org.broadinstitute.gpp.poolq3.barcode

import org.broadinstitute.gpp.poolq3.parser.{CloseableIterable, CloseableIterator, DmuxedIterable}
import org.broadinstitute.gpp.poolq3.types.{Read, ReadIdCheckPolicy}

class DmuxedPairedEndBarcodeSource(
    rowParser: DmuxedIterable,
    revRowParser: DmuxedIterable,
    rowPolicy: BarcodePolicy,
    revRowPolicy: BarcodePolicy,
    umiPolicyOpt: Option[BarcodePolicy],
    readIdCheckPolicy: ReadIdCheckPolicy,
    colBarcodeLength: Int
) extends CloseableIterable[Barcodes]:

  // the index barcode _is_ the column barcode; we get it from the row parser
  // because the demultiplexed file is associated with the index barcode
  private def colBarcodeOpt = rowParser.indexBarcode

  private class BarcodeIterator(rowIterator: CloseableIterator[Read], revRowIterator: CloseableIterator[Read])
      extends CloseableIterator[Barcodes]:

    // used to attempt to parse barcodes out of ids if the file has no associated barcode
    private val colBarcodeParser = Dmuxed.barcodeFromId(colBarcodeLength)

    final override def hasNext: Boolean = rowIterator.hasNext && revRowIterator.hasNext

    final override def next(): Barcodes =
      val nextRow = rowIterator.next()
      val nextRevRow = revRowIterator.next()
      readIdCheckPolicy.check(nextRow, nextRevRow)
      val rowBarcodeOpt = rowPolicy.find(nextRow)
      val revRowBarcodeOpt = revRowPolicy.find(nextRevRow)
      val umiBarcodeOpt = umiPolicyOpt.flatMap(_.find(nextRow))
      Barcodes(rowBarcodeOpt, revRowBarcodeOpt, colBarcodeOpt.orElse(colBarcodeParser(nextRow.id)), umiBarcodeOpt)

    end next

    final override def close(): Unit =
      try rowIterator.close()
      finally revRowIterator.close()

  end BarcodeIterator

  override def iterator: CloseableIterator[Barcodes] =
    new BarcodeIterator(rowParser.iterator, revRowParser.iterator)

end DmuxedPairedEndBarcodeSource
