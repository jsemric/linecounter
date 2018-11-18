#!/usr/bin/env scala
/*
TODO
    test
    parallel
*/

import scala.io.Source
import scala.util.matching.Regex
import java.io._

object LineCounter {

    trait Monoid[A] {
        def op(a: A, b: A): A
        def zero: A
    }

    val intAddition = new Monoid[Int] {
        def op(a: Int, b: Int) = a+b
        def zero = 0
    }

    // monoid for merging two maps
    def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
        new Monoid[Map[K, V]] {
          def zero = Map[K,V]()
          def op(a: Map[K, V], b: Map[K, V]) =
            (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
              acc.updated(k, V.op(a.getOrElse(k, V.zero),
                                  b.getOrElse(k, V.zero)))
            }
        }

    val usage = 
        "Usage: cloc [options]\n" +
        "---------------------\n" +
        "Options:\n" +
        "  -h --help     : show help and exit\n" +
        "  -d <DIR>      : specify directory (default current directory)\n" +
        "  -i <PATTERN>  : ignore files"

    val exts = List(
        "py","R","scala","java","c","cc","cpp","h","hpp","cxx","hxx","hh"
    )

    def main(args: Array[String]) {

        def showHelp = {println(usage); System.exit(1); Map[Symbol, Any]()}

        val arglist = args.toList
        type OptionMap = Map[Symbol, Any]

        def getArgs(map: OptionMap, list: List[String]): OptionMap = {
            list match {
                case Nil => map
                case "-i" :: v :: t => getArgs(map ++ Map('ignore -> v), t)
                case "-d" :: v :: t => getArgs(map ++ Map('dir -> v), t)
                case "-h" :: _ => showHelp
                case "--help" :: _ => showHelp
                case _ => println("Wrong option"); System.exit(1); Map()
            }
        }

        val map = getArgs(Map(),arglist)
        val ignore = map.getOrElse('ignore, "").asInstanceOf[String]
        val dir = map.getOrElse('dir, ".").asInstanceOf[String]

        // val res = traverse(dir,Map[String,Int]())(lcExt, agg)

        val res = traverse2(dir,ignore)(
            mapMergeMonoid[String,Int](intAddition))(lcExt)

        // println(res)
        println("Extension\tLines")
        res map (x => println(s"${x._1}\t\t${x._2}"))
    }

    // count lines
    def lc(file: File): Int = {
        var cnt = 0
        val buf = Source.fromFile(file.getPath)
        val r = buf.getLines.length
        if (r > 0) r - 1 else r
    }

    // extract an extension from file name
    def getExt(fname: String) = {
        val regex = """.*\.(\w+)""".r
        fname match {
          case regex(ext) if exts contains ext => ext
          case _ => ""
        }
    }

    // count lines of a file with an extension
    def lcExt(file: File): Map[String,Int] = {
        val k = getExt(file.getPath)
        if (k == "")
            Map()
        else
            Map(k -> lc(file))
    }

    // merge 2 maps
    def agg(m1: Map[String,Int], m2: Map[String,Int]): Map[String,Int] =
        m1 ++ m2.map{ case (k,v) => k -> (v + m1.getOrElse(k,0)) }

    def traverse[A](file: File, z: A)(f: File => A, g: (A,A) => A): A = {
        if (file.isDirectory)
            file.listFiles.foldLeft(z)((b, a) => g(traverse(a,z)(f,g),b))
        else
            f(file)
    }

    def traverse[A](file: String, z: A)(f: File => A, g: (A,A) => A): A =
        traverse(new File(file), z)(f,g)

    // traverse via monoid
    def traverse2[A](file: File, re: String)(m: Monoid[A])(f: File => A): A = {
        if (file.isDirectory) {
            // filter ignored files            
            val fnames = if (re == "") file.listFiles else
                file.listFiles.filterNot(_.getName matches re)

            // split and recursively traverse
            val (l,r) = fnames.splitAt(fnames.length / 2)
            m.op(
                l.foldLeft(m.zero)((b,a) => m.op(traverse2(a, re)(m)(f), b)),
                r.foldLeft(m.zero)((b,a) => m.op(traverse2(a, re)(m)(f), b))
            )
        }
        else
            f(file) // do something with the file
    }

    def traverse2[A](file: String, re: String)(m: Monoid[A])(f: File => A): A =
        traverse2(new File(file), re)(m)(f)
}