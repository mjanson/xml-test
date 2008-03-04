/* Copyright (c) 2008 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


package com.google.xmldiff

import scala.xml._

/**
 * Main class for xmldiff like tool. It has the following features:
 *  - check that 'expected' document is included in 'actual'. All elements and attributes
 *    have to be present in the 'actual' document, maybe in a different order.
 *  - '-notext' ignores all text nodes. Only document structure matters, contents of elements
 *    and attributes is skipped.
 *  - '-i' allows to specify a list of XPath-like paths to be ignored during the diff. It
 *    understands only '/' and '//', and element names have no namespace (they match any
 *    namespace). For instance, -i //updated feed/author will ignore any updated element,
 *    and /feed/author elements (all paths are absolute).
 * 
 * @author Iulian Dragos
 */
object Main {

  /** The XPath from the root node. */
  var path: List[Elem] = Nil
  
  /** List of XPath-like expressions to be ignored */
  var ignorePaths: List[SimplePath] = Nil
  
  /** Ignore text nodes? */
  var notext = false
  
  /** First document (expected). */
  var xml1: Elem = _ 
  
  /** Second document (expected). */
  var xml2: Elem = _ 
  
  /**
   * Compare a list of nodes to another. Element order does not matter, but
   * other nodes are required to be in the same order.
   */
  def compareElems(exp: List[Node], act: List[Node]): XmlDiff = {
    var es = exp
    var fs = act
    
    while (es != Nil) {
      es.head match {
        case e1: Elem =>
          if (ignored(e1.label)) {
            es = es.tail
          } else {
            val others = fs filter {
              case e2: Elem => sameElem(e1, e2)
              case _ => false
            }
            val results = others.map(compare(e1, _))
            val theGoodOne = results.find(_.isSimilar)
            if (theGoodOne.isEmpty) {
              if (results.isEmpty)
                return error(e1)
              else
                return Diff(path, "None of the elements found fully matches <" 
                    + e1.label + ">: \n\t" + results.mkString("", "\n\t", ""))
            } else {
              es = es.tail
              fs = fs.remove(_ == theGoodOne.get)
            }
          }
          
        case Text(t) if (t.trim.length == 0) => 
          es = es.tail // ignore whitespace
          
        case _ => 
          val res = compare(es.head, fs.head)
          if (!res.isSimilar) return res
          es = es.tail
          fs = fs.tail
      }
    }
    NoDiff
  }
  
  private def error(exp: Node): XmlDiff = {
    val sb = new StringBuilder
    sb.append("Expected: ")
    exp.nameToString(sb)
	Diff(path, sb.toString)
  }
  
  private def wrongAttributes(pref: String, e: Elem, exp: MetaData, actual: MetaData): XmlDiff = {
    val sb = new StringBuilder(64)
	sb.append(pref)
	e.nameToString(sb)
	  .append("\n\tExpected: ").append(exp)
	  .append("\n\tFound: ").append(actual)
	Diff(path, sb.toString)
  }

  /** Returns true if 'label' is an ignored element. */
  private def ignored(label: String): Boolean = {
    val ps = label :: (path map (_.label))
    ignorePaths.exists(_.matches(ps.reverse))
  }
  
  /** Returns 'true' if e1 and e2 have the same qualified name, or e1 is ignored. */
  def sameElem(e1: Elem, e2: Elem): Boolean =
    (ignored(e1.label) 
        || (e1.label == e2.label
            && e1.scope.getURI(e1.prefix) == e2.scope.getURI(e2.prefix)))

  /** Returns 'true' if the attributes in e1 are included in e2.  */
  private def includesAttributes(e1: Elem, e2: Elem): Boolean = {
    
    def contains(a: MetaData) = {
      val attr =
        if (a.isPrefixed) 
          e2.attributes(a.getNamespace(e1), e2.scope, a.key)
        else 
          e2.attributes(a.key)
      (attr != null) && (notext || attr == a.value)
    }
    
    e1.attributes.forall(contains(_))
  }

  /**
   * Compare 'expected' to 'actual'. Returns 'NoDiff' if the attributes and content 
   * (including children) of 'expected' exist and are the same in 'actual'. 'actual'
   * may have additional elements/attributes. Comments are ignored, whitespace is 
   * trimmed.
   */
  def compare(expected: Node, actual: Node): XmlDiff = {
    (expected, actual) match {
      case (Comment(_), _) | (_, Comment(_)) => 
        NoDiff  // ignore comments
        
      case (Text(t1), Text(t2)) => 
        if (notext || t1.trim == t2.trim) 
          NoDiff 
        else
          Diff(path, "Expected " + t1 + " but " + t2 + " found.")
        
      case (e1: Elem, e2: Elem) =>
        path = e1 :: path

        val res = 
          if (ignored(e1.label)) 
            NoDiff 
          else if (sameElem(e1, e2)) {
            if (includesAttributes(e1, e2))
              compareElems(e1.child.toList, e2.child.toList)
            else
              wrongAttributes("Attributes are different at ", e1, e1.attributes, e2.attributes)
          } else {
            val sb = new StringBuilder(128)
            sb.append("Expected ")
            e1.nameToString(sb)
            sb.append(" but ")
            sb.append(e2.nameToString(sb))
            Diff(path, sb.toString)
          }
        path = path.tail
        res
    }
  }
  
  def main(args: Array[String]) {
    parseArgs(args.toList)
    compare(xml1, xml2) match {
      case NoDiff => println("Documents are similar.")
      case diff   => println(diff)
    }
  }
  
  private def parseArgs(args: List[String]) {
    if (args.isEmpty) printUsage
    
    var as = args
    while (as != Nil) as match {
      case "-notext" :: rest =>
        notext = true
        as = rest

      case "-i" :: rest =>
        ignorePaths = for (p <- rest) yield new SimplePath(p)
        as = Nil
        
      case file1 :: file2 :: rest =>
        xml1 = XML.loadFile(file1)
        xml2 = XML.loadFile(file2)
        as = rest

      case _ =>
        printUsage
    }
  }
  
  private def printUsage {
    println("""
xmldiff expected.xml actual.xml [-notext] [-i ignores]

    -notext Ignore text nodes. Only document structure matters (elements and attributes).
    -i      Provide a list of XPath elements to be ignored. Understands only '/' and '//',
            and has no namespace support.

    Compares the two documents for similarity. Element order is not signifficant,
    all other nodes are ordered. This is NOT a two-way diff tool. It checks that <expected.xml>
    is included in <actual.xml>. For a stricter notion of similarity, run the tool twice and
    switch the order of the arguments.
""")
   System.exit(0)
  }
}

abstract class XmlDiff {
  def isSimilar = true
}

case object NoDiff extends XmlDiff

/** The difference between two nodes. */
case class Diff(path: List[Elem], err: String) extends XmlDiff {
  override def isSimilar = false
  override def toString = {
    val sb = new StringBuilder
    sb.append("Diff at ")
    for (p <- path.reverse) p.nameToString(sb.append('/'))
    sb.append(": ").append(err)
    sb.toString
  }
  
}
