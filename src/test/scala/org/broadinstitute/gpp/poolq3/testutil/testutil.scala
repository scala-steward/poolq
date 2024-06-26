/*
 * Copyright (c) 2024 The Broad Institute, Inc. All rights reserved.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
package org.broadinstitute.gpp.poolq3.testutil

import java.nio.file.Path as JPath

import scala.io.Source
import scala.util.Using

import cats.effect.Resource
import fs2.io.file.Files

def contents(p: JPath): String = Using.resource(Source.fromFile(p.toFile))(_.mkString)

def tempFile[F[_]: Files](prefix: String, suffix: String): Resource[F, JPath] =
  Files[F].tempFile(None, prefix, suffix, None).map(_.toNioPath)
