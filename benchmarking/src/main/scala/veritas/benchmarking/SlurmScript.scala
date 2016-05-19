package veritas.benchmarking

import java.io.{File, FileInputStream, FileOutputStream, PrintWriter}
import java.text.SimpleDateFormat
import java.util.Date


/**
  * Class for printing a standard SLURM jobscript which can be run on the HHLR
  *
  * @param timeout       : in seconds
  * @param arraymaxindex : if script is a jobarray: maximum index of array (if not, pass 0)
  * @param provercall    : complete command for calling a prover
  */
case class SlurmScript(jobname: String, stdoutpath: String, stderrpath: String, timeout: Int, provercall: String, arraymaxindex: Int = 0) {

  // fixed script values, adapt if necessary
  val scripttag = "#SBATCH"

  val projectname = "project00184"
  val mailuser = "grewe@st.informatik.tu-darmstadt.de"
  val mailtype = "ALL"

  val tasknumber = 1
  val corespertask = 1
  val mempercpu = 2000
  //in MB
  val features = "avx2"
  val benchmark_conf = true //set to false if benchmark configuration from HHLR is not supposed to be used

  //format timeout in seconds as hh:mm:ss
  private def timeoutToFormat(): String = {
    val formatter: SimpleDateFormat = new SimpleDateFormat("HH:mm:ss")
    formatter.setTimeZone(java.util.TimeZone.getTimeZone("GMT"))
    return formatter.format((new Date(timeout * 1000))) //Date takes timeout in milliseconds!
  }

  override def toString() = {
    val scriptheader =
      s"""
         |#!/bin/bash
         |$scripttag -J $jobname
         |
       """.stripMargin

    val arrayconf = if (arraymaxindex > 0)
      s"""
         |$scripttag -a 1-$arraymaxindex
       """.stripMargin
    else ""

    val stdoutpathA = if (arraymaxindex > 0)
      s"""{$stdoutpath}_%a"""
    else
      s"""$stdoutpath"""

    val stderrpathA = if (arraymaxindex > 0)
      s"""{$stderrpath}_%a"""
    else
      s"""$stderrpath"""

    val config =
      s"""
         |$scripttag -A $projectname
         |$scripttag --mail-user=$mailuser
         |$scripttag --mail-type=$mailtype
         |$scripttag -o $stdoutpathA
         |$scripttag -e $stderrpathA
         |$scripttag -t ${timeoutToFormat()}
         |$scripttag --mem-per-cpu=$mempercpu
         |$scripttag-n $tasknumber
         |$scripttag -c $corespertask
         |$scripttag -C $features
     """.stripMargin

    val benchmark = if (benchmark_conf) s"""\n$scripttag -p benchmark \n""" else ""

    scriptheader + arrayconf + config + benchmark + provercall + "\n"

  }

  def writeScriptToFile(filepath: String) = {
    val filehandler = new File(filepath)
    if (!filehandler.getParentFile.exists())
      filehandler.getParentFile.mkdirs()
    filehandler.createNewFile()
    new PrintWriter(filehandler) {
      write(toString());
      close
    }
  }
}

case class SlurmScriptMaker(proverconfigs: Seq[ProverConfig], provertimeout: Int, flatIndexFileMap: Map[Int, File]) {

  val pathforHHLRInput = "datasets/HHLRInputFiles/"
  val pathforHHLRJobscripts = "datasets/HHLRJobScripts/"

  val timeoutbuffer = provertimeout * 2 //buffer time which is used when making scripts

  def writeFlattenedFileStructure() = {
    for ((i, f) <- flatIndexFileMap)
      {
        val destfile = new File(s"${pathforHHLRInput}proverinput_$i")
        new FileOutputStream(destfile) getChannel() transferFrom(new FileInputStream(f) getChannel, 0, Long.MaxValue)
      }

  }

  def writeJobScripts() = {
    for (pc <- proverconfigs)
      {
        val jobsize = flatIndexFileMap.size
        val arraymax = if (jobsize > 1) jobsize else 0
        val jobname = pc.name
        val stdoutpath = ??? // s"./$jobname" //TODO
        val stderrpath = stdoutpath
        val slurmjob = SlurmScript(jobname, stdoutpath, stderrpath, timeoutbuffer, ???, arraymax)
        slurmjob.writeScriptToFile(pathforHHLRJobscripts + jobname)
      }
  }

}