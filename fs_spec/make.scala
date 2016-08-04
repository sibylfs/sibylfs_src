// #!/bin/sh 
// exec scala -save $0 $@
// !#

// to compile with sbt, comment above

// to run directly: scala -save make.scala

import scala.sys.process._
import java.io._

import scala.collection.immutable.StringOps

object Make_sibylfs {

  /******************** scala lib */

  def debug(s:String) = {}

  implicit class Any_with_rev_app[B](x:B) {
    def |>[A](f:B => A) = {f(x)}
  }

  def failwith(s:String) : Nothing = { throw new Exception(s) }

  def s_to_lines(s:String) : List[String] = s.linesIterator.toList // or split on whitespace?

  val whitespace = "\\s+"

  import scala.language.implicitConversions
  def s_to_ss(s:String) : List[String] = s.split(whitespace).toList

  def remove_blank_lines(x:List[String]) = x.filter( s => 
    !(s.matches(whitespace) || s == ""))

  implicit def s_to_list_string(s:String): List[String] = s|>s_to_ss|>remove_blank_lines


  /******************** script lib */

  // we need an implicit conversion from string to file
  import scala.language.implicitConversions

  implicit def s_to_f(s:String)(implicit c: Context) : File = {
    val wd = if (c.wd.endsWith("/")) c.wd else c.wd+"/" // note that we always add a /
    if(s.startsWith("/")) (new File(s))
    else (new File(wd+s))
  }

  def suffix(s:String) = (s:StringOps).drop(s.lastIndexOf("."))

  // abc.ext => (abc,ext)
  def split_filename(s:String) = {
    val n = s.lastIndexOf(".")
    ((s:StringOps).take(n),(s:StringOps).drop(n+1))
  }

  implicit class My_rich_string(s:String) {
    def from(a:String) = s.stripSuffix(a)
    def to(b:String) = s+b
    def /(t:String) = s+"/"+t
    def ext() = suffix(s)
  }


  case class Context(wd:String=".")

  var exec_log = ""

  def execb(echo:Boolean,s:String)(implicit c:Context) : List[String] = {
    exec_log = exec_log + s"""(cd ${c.wd} && $s)\n"""
    if(echo) println("#exec: "+c.wd+"\n"+s)
    // println(s)
    val result : String = { Process(s,new File(c.wd)).!! }
    debug("#exec result: \n"+result)
    result |> s_to_lines
  }

  def exec(s:String)(implicit c:Context) : List[String] = execb(true,s)(c)

  // no echo
  def execq(s:String)(implicit c:Context) : List[String] = execb(false,s)(c)


  def execs(t:String)(implicit c:Context) : List[String] = {
    val ss = s_to_lines(t) |> remove_blank_lines
    ss.flatMap(f => exec(f)(c))
  }

  // true if any dst does not exist, or some src is newer than some dst
  def newer(src:List[String],dst:List[String])(implicit c:Context) : Boolean = {
    // check if all dst exist
    val not_exists = dst.find(f => !(f:File).exists)
    not_exists match {
      case Some(x) => { debug(s"newer: $x not present"); true}
      case None => {
        val dst_min = {Long.MaxValue :: (dst.map(f => (f:File).lastModified())) }.min
        val src_newer = src.find(f => (f:File).lastModified() > dst_min)
        src_newer match {
          case None => false
          case Some(x) => { debug(s"newer: src $x is newer"); true }
        }
      }
    }
  }

  // to record dependencies
  case class Dep(dst:String,srcs:List[String],comm:String="")

  // we use the following concrete syntax
  // a.cmo: a.ml b.cmi c.cmo # camlp4o
  val dep_re = "([^:]*):([^#]*)[#]?(.*)".r
  def split_dep(s:String) : Dep = {
    s match {
      case dep_re(dst,srcs,comm) => Dep(dst.trim(),
        srcs.trim() : List[String], comm.trim())
      case _ => failwith(s"failed to parse dependency line $s")
    }
  }

  def string_to_deps(s:String) : List[Dep] = 
    { s |> s_to_lines |> remove_blank_lines |> { _.map(split_dep(_)) } }

  implicit class Rich_deps(deps:List[Dep]) {
    val deps_map_list : List[(String,(List[String],String))] = 
      deps.map(d => (d.dst,(d.srcs,d.comm)))

    val deps_map = Map() ++ deps_map_list

    val keys = deps_map.keys.toList

    def comments(k:String) = deps_map(k)._2

    // get_deps("abc",".cmi") returns deps of abc, with .cmi extensions
    def get_deps(fn:String,ext:String) : List[String] = {
      deps_map(fn)._1.map(_.to(ext))
    }
  }


  /******************** build function */

  case class Make_params(
    c0: Context,
    deps: List[Dep],
    mli_files: List[String], // the mli files that exist in src
    ocamlc: String = "ocamlfind ocamlc", // command to run ocamlc
    ocamlopt: String = "ocamlfind ocamlopt",
    native:Boolean = true,
    extra_flags: String => String = {(x) => ""}
  )

  def get_make(p0:Make_params) = {
    import p0._
    implicit val c = p0.c0
    // we never try to make the same thing twice
    val cc = if (native) ocamlopt else ocamlc
    var so_far :Set[String] = Set()
    def make(s:String) : Unit = {
      (so_far.contains(s)) match {
        case true => ()
        case false => {
          // println(s"make: $s")
          // we try to build
          val (f,ext) = split_filename(s)
          ext match {
            case ("cmo" | "cmx") => {
              // to build a cmo, we need the cmi of all the dependents
              val cmi_deps = deps.get_deps(f,".cmi")
              cmi_deps.map(make _)
              // if the .mli file exists, make this first
              if (mli_files.contains(f+".mli")) make(f+".cmi")
              val cc = if (ext == "cmo") ocamlc else ocamlopt
              if(newer((f+".ml")::cmi_deps,f+"."+ext)) // FIXME make ext start with .?
                exec(s"""$cc ${extra_flags(f)} -c $f.ml""")
              else
                println(s"$s up-to-date")
            }
            case "cmi" => {
              val cmi_deps = deps.get_deps(f,".cmi")
              cmi_deps.map(make _)
              // if there is an mli, we can compile that, otherwise try to build .cm[ox]
              val f_mli = f+".mli"
              if (mli_files.contains(f_mli)) {
                if (newer(f_mli::cmi_deps,f+".cmi")) {
                  exec(s"""$cc ${extra_flags(f)} -c $f_mli""") // FIXME ocamlc or opt?
                } else
                  println(s"$s up-to-date")
              } else {
                val f_cm = if (native) (f+".cmx") else (f+".cmo")
                if(newer(f+".ml"::cmi_deps,f+".cmi")) {
                  exec(s"rm -f $f_cm") // we want to force rebuild of cmi
                  make(f_cm) // cmi as a side effect
                } else println(s"$s up-to-date")
              }
            }
            case _ => failwith(ext)
          }
          so_far=so_far+s
        }
      }
    } // make
    make _
  } // get_make


  /******************** sibylfs specific */
  object sibylfs {

    /******************** env vars */

    // we assume these are all set appropriately and eg extract.cma exists
    // we assume ocamlfind is available, with required libs
    val env_vars : List[String] = "LEM LEMLIB EXTRACTDIR CPPO SIBYLFS_CONFIG";
    val (lem,lemlib,extractdir,cppo) = {
      val vs = env_vars.map(s => sys.env.getOrElse(s,failwith(s"$s not found in environment")))
      vs match {
        case x::y::z::w::_ => (x,y,z,w)
        case _ => failwith("impossible")
      }
    }

    // // FIXME
    // val (lem,lemlib,extractdir,cppo) = (
    //   "/nix/store/25fb9v3c69lv0l4ymcc0k52n86kj7m6l-lem/lem/lem",
    //   "/nix/store/25fb9v3c69lv0l4ymcc0k52n86kj7m6l-lem/lem/library",
    //   "/nix/store/25fb9v3c69lv0l4ymcc0k52n86kj7m6l-lem/lem/ocaml-lib/_build",
    //   "/nix/store/lbfqdl6bxaqm6mzhwcqv9szn09lzzycj-cppo-1.3.2/bin/cppo"
    // )

    val cppo_args = sys.env.getOrElse("CPPO_ARGS","-D aspect_perms")


    /******************** sibylfs config, dependencies */

    //val root = "/tmp/l/data/git/github/sibylfs_src"
    val fs_spec = "." // "/tmp/l/sibylfs_src/fs_spec" // //root/"fs_spec" FIXME or cwd
    val src = fs_spec/"src"

    // here we record the dependencies; presumably this includes all the tml_to_ml files
    val deps : List[Dep] = string_to_deps("""
abstract_string : # camlp4o
lem_support : 
list_array : abstract_string
fs_prelude : lem_support
fs_dict_wrappers : fs_prelude lem_support
fs_spec : list_array abstract_string fs_prelude lem_support  # camlp4o
dir_heap : fs_spec list_array fs_prelude lem_support # camlp4o
fs_dump : dir_heap list_array fs_spec fs_dict_wrappers # camlp4o
fs_printer : list_array fs_spec fs_dict_wrappers lem_support # camlp4o
fs_interface : fs_dump dir_heap fs_printer fs_spec abstract_string lem_support # camlp4o
""")

    val lemflags = "-only_changed_output -wl_unused_vars ign -wl_rename err"

    val pkgs = "-package sexplib,sexplib.syntax,sha"
    val ocamlc = s"ocamlfind ocamlc -I $extractdir extract.cma $pkgs"
    val ocamlopt = s"ocamlfind ocamlopt -I $extractdir extract.cmxa $pkgs"


    /******************** sibylfs build */

    val src_files : List[String] = { execq("ls")(Context(src)) }.filter( s =>
      ".lem_cppo .lem .mli .ml .sed".contains(s.ext))

    def srcs(s:String) = src_files.filter(_.ext ==  s)

    // NB the original mli in src
    val mli_files : List[String] = src_files.filter(_.ext == ".mli")

    def mk_build_dir() = { 
      implicit val c0 = (Context(fs_spec))
      if(newer(Nil,"_build"))
        exec("mkdir -p _build") 
    }

    def clean() = { exec("rm -rf _build")(Context(fs_spec)) }


    // the following all takes place in _build
    implicit val c0 = Context(fs_spec/"_build")

    def link() : Unit = {
      // only link if not already present
      src_files.map( f => {
        if(newer("../src"/f,f))
          execq(s"ln -sf ../src/$f  .") // quiet
      })
    }

    // map t_xxx.lem_cppo to t_xxx.lem
    def cppo_to_lem() : Unit = {
      srcs(".lem_cppo")
        .map(_.from(".lem_cppo"))
        .map(f =>
        if(newer(f+".lem_cppo",f+".lem"))
          execq(s"$cppo $cppo_args $f.lem_cppo -o $f.lem") // quiet
        else println(s"$f.lem up-to-date")
      )
    }

    // run lem
    def run_lem() = {
      val lems = execq("ls").filter(_.ext==".lem")
      if(newer(lems,"run_lem")) { //
        execs(s"""
$lem -lib $lemlib $lemflags -ocaml ${lems.mkString(" ")}
sed -i.bak -f patch_lem.sed t_fs_spec.ml
touch run_lem
""")
      } else println("run_lem up-to-date")
    }

    // FIXME should remove all this indirection via t_? no, probably
    // makes sense to keep it, given need for patching

    def rename_tml() = {
      // need this inside function, otherwise may be evaluated too early
      val tml :  List[String] = execq("ls").filter(_.startsWith("t_")).filter(_.ext==".ml")
      // copy and patch each file
      tml.map{ f => {
        val g = f.from(".ml").stripPrefix("t_")
        if(newer(s"t_$g.ml",s"$g.ml")) {
          execq(s"cp t_$g.ml $g.ml")
          execq(s"sed -i.bak -f patch_gen_ml.sed $g.ml")
        } else println(s"$g.ml up-to-date")
      }}
      
    }

    val ml_files = deps.keys.map(_.to(".ml")) // assume they are all in the map

    val deps_map_keys = deps.keys.sorted

    val p0 = Make_params(
      c0,
      deps,
      mli_files,
      ocamlc,
      ocamlopt,
      true,
      { (s) => if (deps.comments(s).contains("camlp4o")) "-syntax camlp4o" else ""})

    // targets are only the ocaml targets
    def make(xs:List[String]) : Unit = {
      val m = get_make(p0)
      xs.map(f => m(f))
    }

    def make_all() = {
      mk_build_dir()
      link()
      cppo_to_lem()
      run_lem()
      rename_tml()
      val targets = 
        //deps.keys.flatMap( f => (f.to(".cmi")::f.to(".cmo")::f.to(".cmx")))
        deps.keys.flatMap( f => (f.to(".cmi")::f.to(".cmx")))
      make(targets)
    }

    def dot() : String = {
      var edges = ""
      def g(f:String) : Unit = { // s is the target
        val ds = deps.deps_map(f)._1.mkString(" ")
        edges=edges+s"$f -> { $ds } \n"
      }
      deps_map_keys.map(g _)
      s"digraph {\n$edges\n}"
    }

  } // sibylfs

  def main(args:Array[String]) {
    val xs = args.toList
    xs match {
      case Nil => sibylfs.make_all
      case List("clean") => sibylfs.clean
      case _ => sibylfs.make(xs.toList)
    }
    println(exec_log)
  }


}
