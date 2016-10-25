package veritas.benchmarking

import java.io.{File, FileInputStream, FileOutputStream, PrintWriter}
import java.text.SimpleDateFormat
import java.util.Date


/**
  * Class for printing a standard SLURM jobscript which can be run on the HHLR
  *
  * @param timeout       : in seconds
  * @param arraymaxindex : if script is a jobarray: maximum index of array (if not, pass 0)
  * @param commands      : complete command for calling a prover
  */
case class SlurmScript(jobname: String, stdoutpath: String, stderrpath: String, timeout: Int, commands: String, arraymaxindex: Int = 0) {

  // fixed script values, adapt if necessary
  val scripttag = "#SBATCH"
  val maxarraysize: Int = 1000
  val filenumber = "filenumber"

  val projectname = "project00184"
  val mailuser = "grewe@st.informatik.tu-darmstadt.de"
  val mailtype = "ALL"

  val tasknumber = 1
  val corespertask = 4
  val mempercpu = 2000
  //in MB
  val features = "avx2"
  val benchmark_conf = "fixfreq" //values: "" - no special partition, "benchmark": exclusive use of nodes, "fixfreq": use nodes in benchmark partition, but no exclusive node use per job

  //format timeout in seconds as hh:mm:ss
  private def timeoutToFormat(): String = {
    val formatter: SimpleDateFormat = new SimpleDateFormat("HH:mm:ss")
    formatter.setTimeZone(java.util.TimeZone.getTimeZone("GMT"))
    return formatter.format((new Date(timeout * 1000))) //Date takes timeout in milliseconds!
  }

  def scriptString(): Seq[String] = {

    //distribute array in batches with maximum size 1000
    val minnumberOfScripts: Int = arraymaxindex / maxarraysize
    val numberOfScripts: Int = if (minnumberOfScripts * 1000 <= arraymaxindex) minnumberOfScripts + 1 else minnumberOfScripts

    for (i <- 0 until numberOfScripts) yield {
      val scriptheader = s"""#!/bin/bash
        |$scripttag -J $jobname
        |""".stripMargin

      val localmaxindex = if ((i+1) < numberOfScripts) 1000 else arraymaxindex % maxarraysize
      val arrayconf = if (arraymaxindex > 0)
        s"""$scripttag -a 1-$localmaxindex
          |""".stripMargin
      else ""

      val stdoutpathA = if (arraymaxindex > 0)
          s"""${stdoutpath}_${i}_%a"""
      else
          s"""$stdoutpath"""

      val stderrpathA = if (arraymaxindex > 0)
        s"""${stderrpath}_${i}_%a"""
      else
        s"""$stderrpath"""

      val config =
        s"""$scripttag -A $projectname
           |$scripttag --mail-user=$mailuser
           |$scripttag --mail-type=$mailtype
           |$scripttag -o $stdoutpathA
           |$scripttag -e $stderrpathA
           |$scripttag -t ${timeoutToFormat()}
           |$scripttag --mem-per-cpu=$mempercpu
           |$scripttag -n $tasknumber
           |$scripttag -c $corespertask
           |$scripttag -C $features
      """.stripMargin

      val benchmark = if (benchmark_conf != "") s"""\n$scripttag -p $benchmark_conf \n""" else ""
      val calcindex = if (arraymaxindex == 0) s"$filenumber=$$SLURM_ARRAY_TASK_ID\n" else s"$filenumber=$$(($i*1000+SLURM_ARRAY_TASK_ID))\n"
      scriptheader + arrayconf + config + benchmark + calcindex + commands + "\n"
    }

  }

  def writeScriptToFile(filepath: String) = {
    val scriptstrings = scriptString()
    for (i <- 0 until scriptstrings.size) {
      val filehandler = new File(filepath + "_" + i)
      if (!filehandler.getParentFile.exists())
        filehandler.getParentFile.mkdirs()
      filehandler.createNewFile()
      new PrintWriter(filehandler) {
        write(scriptstrings(i));
        close
      }
    }
  }
}

case class SlurmScriptMaker(proverconfigs: Seq[ProverConfig], provertimeout: Int, flatIndexFileMap: Map[Int, File]) {

  val currentdate: String = (new SimpleDateFormat("yyyy-MM-dd")).format(new Date())
  val pathforHHLRInput = s"datasets/HHLRInputFiles/$currentdate/"

  val proverpath = "/home/groups/projects/proj_184/provers/"
  val inputfilename = "proverinput"
  val inputpathHHLR = "/home/groups/projects/proj_184/Encodings/"
  val outputfilename = "proveroutput"
  val outputpathHHLR = "/work/scratch/groups/projects/proj_184/"
  val pathforHHLRJobscripts = "datasets/HHLRJobScripts/"

  val arrayindexref = "filenumber"

  val timeoutbuffer = provertimeout * 2 //buffer time which is used when making scripts

  def writeFlattenedFileStructure() = {
    for ((i, f) <- flatIndexFileMap) {
      val destfile = new File(s"$pathforHHLRInput${inputfilename}_$i")
      if (!destfile.getParentFile.exists())
        destfile.getParentFile.mkdirs()
      new FileOutputStream(destfile) getChannel() transferFrom(new FileInputStream(f) getChannel, 0, Long.MaxValue)
    }

  }

  def writeJobScripts() = {
    for (pc <- proverconfigs) {
      val jobsize = flatIndexFileMap.size
      val arraymax = if (jobsize > 1) jobsize else 0
      val jobname = pc.name
      val stdoutpath = s"$outputpathHHLR$currentdate/${provertimeout}s/$jobname/${outputfilename}"
      val stderrpath = stdoutpath
      val inputpath = if (jobsize > 1) s"$inputpathHHLR$currentdate/${inputfilename}_$$$arrayindexref" else s"$inputpathHHLR$currentdate/${inputfilename}"

      val moduleloads = pc.modulesToLoad.map("module load " + _).mkString("\n") + "\n"
      val provercall = pc.makeCall(new File(inputpath), provertimeout, false)
      val provercallHHLR = pc.createProverCallHHlr(proverpath, provercall)


      val slurmjob = SlurmScript(jobname, stdoutpath, stderrpath, timeoutbuffer, moduleloads + provercallHHLR, arraymax)
      slurmjob.writeScriptToFile(pathforHHLRJobscripts + jobname)
    }
  }

}