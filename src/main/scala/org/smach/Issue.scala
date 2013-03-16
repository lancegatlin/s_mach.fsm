/*
    Copyright 2013 Lance Gatlin

    Author: lance.gatlin@gmail.com

    This file is part of org.smach library.

    org.smach library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    org.smach library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with org.smach library. If not, see <http://www.gnu.org/licenses/>.

*/
package org.smach

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