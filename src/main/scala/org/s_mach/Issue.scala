/*
    Copyright 2013 Lance Gatlin

    Author: lance.gatlin@gmail.com

    This file is part of org.s_mach library.

    org.s_mach library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    org.s_mach library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with org.s_mach library. If not, see <http://www.gnu.org/licenses/>.

*/
package org.s_mach

import IssueSeverityCode._

case class Issue(
  severityCode    :   IssueSeverityCode,
  message         :   String,
  cause           :   Option[Throwable] = None
)

object Issue {
  def debug(message : String, cause : Option[Throwable] = None) = Issue(DEBUG, message, cause)
  def info(message : String, cause : Option[Throwable] = None) = Issue(INFO, message, cause)
  def warn(message : String, cause : Option[Throwable] = None) = Issue(WARN, message, cause)
  def error(message : String, cause : Option[Throwable] = None) = Issue(ERROR, message, cause)
  def fatal(message : String, cause : Option[Throwable] = None) = Issue(FATAL, message, cause)
}