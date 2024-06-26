/*
 * Copyright (c) 2024 The Broad Institute, Inc. All rights reserved.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package org.broadinstitute.gpp.poolq3.barcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

/** @author
  *   Broad Institute Genetic Perturbation Platform
  */
class KeyRangeTest extends AnyFlatSpec:

  "KeyRange" should "enforce well-formedness" in {
    val _ = noException should be thrownBy KeyRange(3, 4)
    val _ = noException should be thrownBy KeyRange(3, 3)
    val _ = an[IllegalArgumentException] should be thrownBy KeyRange(3, 2)
    an[IllegalArgumentException] should be thrownBy KeyRange(-2, 2)
  }

  it should "have working compare()" in {
    val ord = implicitly[Ordering[KeyRange]]
    val _ = ord.compare(KeyRange(2, 5), KeyRange(2, 5)) should be(0)
    val _ = KeyRange(2, 5) should be <= KeyRange(2, 5)
    val _ = KeyRange(2, 5) should be >= KeyRange(2, 5)

    val _ = KeyRange(2, 5) should be < KeyRange(3, 4)
    val _ = KeyRange(2, 5) should be < KeyRange(2, 6)
    val _ = KeyRange(2, 5) should be > KeyRange(2, 4)
    KeyRange(2, 5) should be > KeyRange(1, 32)
  }

  it should "be creatable from a string" in {
    val _ = KeyRange("1-1") should be(KeyRange(0, 0))
    val _ = KeyRange("1..1") should be(KeyRange(0, 0))
    val _ = KeyRange("1") should be(KeyRange(0, 0))
    val _ = KeyRange("1-6") should be(KeyRange(0, 5))
    val _ = KeyRange("1..6") should be(KeyRange(0, 5))
    val _ = an[IllegalArgumentException] should be thrownBy KeyRange("0-5")
    val _ = an[IllegalArgumentException] should be thrownBy KeyRange("-1-5")
    an[IllegalArgumentException] should be thrownBy KeyRange("6-5")
  }

end KeyRangeTest
