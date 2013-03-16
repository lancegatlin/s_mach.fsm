/*
    Copyright 2012 Georgia Tech Research Institute

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
package org.smach.test

case class LogEntry(level : String, fqcn : String, message : String, cause : Option[Throwable] = None) {
  override def toString : String = s"[$level] $fqcn: $message $cause"
}

class Log(fqcn : String) {
  def info(message: String) = LogEntry("INFO",fqcn,message,None)
  def warn(message: String, cause : Option[Throwable] = None) = LogEntry("WARN",fqcn,message,cause)
  def error(message: String, cause : Option[Throwable] = None) = LogEntry("ERROR",fqcn,message,cause)
  def fatal(message: String, cause : Option[Throwable] = None) = LogEntry("FATAL",fqcn,message,cause)
}
object Log {
  def apply(c: Class[_ <: AnyRef]) : Log = new Log(c.getCanonicalName)
}