package Chainsaw

import net.openhft.affinity.impl.VanillaCpuLayout
import org.apache.commons.io.FileUtils
import spinal.core.internals.{DeclarationStatement, GraphUtils, PhaseContext, PhaseNetlist}
import spinal.core.sim.{SimCompiled, SimPublic, SimWorkspace, SpinalSimConfig, SpinalVerilatorBackendConfig, SwapTagPhase}
import spinal.core.{BaseType, Bits, BlackBox, Bool, Component, InComponent, Mem, MemSymbolesTag, SInt, SpinalEnumCraft, SpinalReport, UInt, Verilator}
import spinal.sim._

import java.io.{File, PrintWriter}
import javax.tools.JavaFileObject
import scala.sys.process._

object MySim {

  implicit class simConfigExtra(simConfig: SpinalSimConfig) {
    def myCompile[T <: Component](rtl: => T): SimCompiled[T] = {

      FileUtils.deleteQuietly(new File(s"${simConfig._workspacePath}/${simConfig._workspaceName}"))
      new File(s"${simConfig._workspacePath}/${simConfig._workspaceName}").mkdirs()

      val config = simConfig._spinalConfig
        .copy(targetDirectory = s"${simConfig._workspacePath}/${simConfig._workspaceName}")
        .addTransformationPhase(new PhaseNetlist {
          override def impl(pc: PhaseContext): Unit = pc.walkComponents {
            case b: BlackBox if b.isBlackBox && b.isSpinalSimWb => b.clearBlackBox()
            case _ =>
          }
        })
      val report = {
        config.addTransformationPhase(new SwapTagPhase(SimPublic, Verilator.public))
        config.generateVerilog(rtl)
      }
      report.blackboxesSourcesPaths ++= simConfig._additionalRtlPath
      report.blackboxesIncludeDir ++= simConfig._additionalIncludeDir
      simConfig.myCompile[T](report)
    }

    def myCompile[T <: Component](report: SpinalReport[T]): SimCompiled[T] = {
      import simConfig._
      if (_workspacePath.startsWith("~"))
        _workspacePath = System.getProperty("user.home") + _workspacePath.drop(1)

      if (_workspaceName == null)
        _workspaceName = s"${report.toplevelName}"

      _workspaceName = SimWorkspace.allocateWorkspace(_workspacePath, _workspaceName)

      println(f"[Progress] Simulation workspace in ${new File(s"${_workspacePath}/${_workspaceName}").getAbsolutePath}")
      new File(s"${_workspacePath}").mkdirs()
      new File(s"${_workspacePath}/${_workspaceName}").mkdirs()
      new File(s"${_workspacePath}/${_workspaceName}/rtl").mkdirs()

      // merge top and enum define
      if (new File(s"${_workspacePath}/${_workspaceName}/enumdefine.v").exists()) {
        val topPath = report.generatedSourcesPaths.head
        val topString = scala.io.Source.fromFile(topPath).getLines().mkString("\n")
        val enumString = scala.io.Source.fromFile(s"${_workspacePath}/${_workspaceName}/enumdefine.v").getLines().mkString("\n")
        val top = new File(topPath)
        FileUtils.write(top, topString + enumString)
      }

      report.generatedSourcesPaths.foreach { srcPath =>
        FileUtils.copyFileToDirectory(new File(srcPath), new File(s"${_workspacePath}/${_workspaceName}/rtl"))
      }

      println(f"[Progress] Verilator compilation started")
      val startAt = System.nanoTime()
      val vConfig = SpinalVerilatorBackendConfig[T](
        rtl = report,
        waveFormat = _waveFormat,
        workspacePath = s"${_workspacePath}/${_workspaceName}",
        vcdPath = s"${_workspacePath}/${_workspaceName}",
        vcdPrefix = null,
        workspaceName = "verilator",
        waveDepth = _waveDepth,
        optimisationLevel = _optimisationLevel,
        simulatorFlags = _simulatorFlags,
        withCoverage = _withCoverage
      )
      val backend = getVerilatorBackend(vConfig)
      val deltaTime = (System.nanoTime() - startAt) * 1e-6
      println(f"[Progress] Verilator compilation done in $deltaTime%1.3f ms")
      new SimCompiled(report) {
        override def newSimRaw(name: String, seed: Int): SimRaw = {
          val raw = new MySimVerilator(backend, backend.instanciate(name, seed))
          raw.userData = backend.config.signals
          raw
        }
      }
    }
  }

  def getVerilatorBackend[T <: Component](config: SpinalVerilatorBackendConfig[T]) = {

    import config._

    val vconfig = new VerilatorBackendConfig()
    vconfig.rtlIncludeDirs ++= rtl.rtlIncludeDirs
    vconfig.rtlSourcesPaths ++= rtl.rtlSourcesPaths
    vconfig.toplevelName = rtl.toplevelName
    vconfig.vcdPath = vcdPath
    vconfig.vcdPrefix = vcdPrefix
    vconfig.workspaceName = workspaceName
    vconfig.workspacePath = workspacePath
    vconfig.waveFormat = waveFormat match {
      case WaveFormat.DEFAULT => WaveFormat.VCD
      case _ => waveFormat
    }
    vconfig.waveDepth = waveDepth
    vconfig.optimisationLevel = optimisationLevel
    vconfig.simulatorFlags = simulatorFlags
    vconfig.withCoverage = withCoverage

    var signalId = 0

    def addSignal(bt: DeclarationStatement with InComponent): Unit = {
      val signal = new Signal(config.rtl.toplevelName +: bt.getComponents().tail.map(_.getName()) :+ bt.getName(), bt match {
        case bt: Bool => new BoolDataType
        case bt: Bits => new BitsDataType(bt.getBitsWidth)
        case bt: UInt => new UIntDataType(bt.getBitsWidth)
        case bt: SInt => new SIntDataType(bt.getBitsWidth)
        case bt: SpinalEnumCraft[_] => new BitsDataType(bt.getBitsWidth)
        case mem: Mem[_] => new BitsDataType(mem.width).setMem()
      })

      bt.algoInt = signalId
      bt.algoIncrementale = -1
      signal.id = signalId
      vconfig.signals += signal
      signalId += 1
    }

    //    GraphUtils.walkAllComponents(rtl.toplevel, c => c.dslBody.walkStatements(s => {
    //      s match {
    //        case bt: BaseType if bt.hasTag(Verilator.public) && !(!bt.isDirectionLess && bt.component.parent == null) => {
    //          addSignal(bt)
    //        }
    //        case mem: Mem[_] if mem.hasTag(Verilator.public) => {
    //          val tag = mem.getTag(classOf[MemSymbolesTag])
    //          mem.algoInt = signalId
    //          mem.algoIncrementale = -1
    //          tag match {
    //            case None => addSignal(mem)
    //            case Some(tag) => {
    //              for (mapping <- tag.mapping) {
    //                val signal = new Signal(config.rtl.toplevelName +: mem.getComponents().tail.map(_.getName()) :+ mapping.name, new BitsDataType(mapping.width).setMem())
    //                signal.id = signalId
    //                vconfig.signals += signal
    //                signalId += 1
    //              }
    //            }
    //          }
    //        }
    //        case _ => {
    //          s.algoInt = -1
    //        }
    //      }
    //    }))

    for (io <- rtl.toplevel.getAllIo) {
      val bt = io
      val btName = bt.getName()
      if (true) {
        val signal = new Signal(bt.getComponents().tail.map(_.getName()) :+ bt.getName(), bt match {
          case bt: Bool => new BoolDataType
          case bt: Bits => new BitsDataType(bt.getBitsWidth)
          case bt: UInt => new UIntDataType(bt.getBitsWidth)
          case bt: SInt => new SIntDataType(bt.getBitsWidth)
          case bt: SpinalEnumCraft[_] => new BitsDataType(bt.getBitsWidth)
        })
        bt.algoInt = signalId
        bt.algoIncrementale = -1
        signal.id = signalId
        vconfig.signals += signal
        signalId += 1
      }
    }
    //    println(vconfig.signals.mkString("\n"))
    new MyVerilatorBackend(vconfig)
    //    new VerilatorBackend(vconfig)
  }

  class MyVerilatorBackend(val config: VerilatorBackendConfig) extends Backend {

    import Backend._

    val workspaceName = config.workspaceName
    val workspacePath = config.workspacePath
    val wrapperCppName = s"V${config.toplevelName}__spinalWrapper.cpp"
    val wrapperCppPath = new File(s"${workspacePath}/${workspaceName}/$wrapperCppName").getAbsolutePath

    def clean(): Unit = {
      FileUtils.deleteQuietly(new File(s"${workspacePath}/${workspaceName}"))
    }

    val availableFormats = Array(WaveFormat.VCD, WaveFormat.FST,
      WaveFormat.DEFAULT, WaveFormat.NONE)

    val format = if (availableFormats contains config.waveFormat) {
      config.waveFormat
    } else {
      println("Wave format " + config.waveFormat + " not supported by Verilator")
      WaveFormat.NONE
    }

    def genWrapperCpp(): Unit = {
      val jniPrefix = "Java_" + s"wrapper_${workspaceName}".replace("_", "_1") + "_VerilatorNative_"
      val wrapperString =
        s"""
#include <stdint.h>
#include <string>
#include <memory>
#include <jni.h>
#include <iostream>

#include "V${config.toplevelName}.h"
#ifdef TRACE
#include "verilated_${format.ext}_c.h"
#endif
#include "V${config.toplevelName}__Syms.h"

using namespace std;

class ISignalAccess{
public:
  virtual ~ISignalAccess() {}

  virtual void getAU8(JNIEnv *env, jbyteArray value) {}
  virtual void getAU8_mem(JNIEnv *env, jbyteArray value, size_t index) {}
  virtual void setAU8(JNIEnv *env, jbyteArray value, int length) {}
  virtual void setAU8_mem(JNIEnv *env, jbyteArray value, int length, size_t index) {}

  virtual uint64_t getU64() = 0;
  virtual uint64_t getU64_mem(size_t index) = 0;
  virtual void setU64(uint64_t value) = 0;
  virtual void setU64_mem(uint64_t value, size_t index) = 0;
};

class  CDataSignalAccess : public ISignalAccess{
public:
    CData *raw;
    CDataSignalAccess(CData *raw) : raw(raw){}
    CDataSignalAccess(CData &raw) : raw(addressof(raw)){}
    uint64_t getU64() {return *raw;}
    uint64_t getU64_mem(size_t index) {return raw[index];}
    void setU64(uint64_t value)  {*raw = value; }
    void setU64_mem(uint64_t value, size_t index){raw[index] = value; }
};


class  SDataSignalAccess : public ISignalAccess{
public:
    SData *raw;
    SDataSignalAccess(SData *raw) : raw(raw){}
    SDataSignalAccess(SData &raw) : raw(addressof(raw)){}
    uint64_t getU64() {return *raw;}
    uint64_t getU64_mem(size_t index) {return raw[index];}
    void setU64(uint64_t value)  {*raw = value; }
    void setU64_mem(uint64_t value, size_t index){raw[index] = value; }
};


class  IDataSignalAccess : public ISignalAccess{
public:
    IData *raw;
    IDataSignalAccess(IData *raw) : raw(raw){}
    IDataSignalAccess(IData &raw) : raw(addressof(raw)){}
    uint64_t getU64() {return *raw;}
    uint64_t getU64_mem(size_t index) {return raw[index];}
    void setU64(uint64_t value)  {*raw = value; }
    void setU64_mem(uint64_t value, size_t index){raw[index] = value; }
};


class  QDataSignalAccess : public ISignalAccess{
public:
    QData *raw;
    QDataSignalAccess(QData *raw) : raw(raw){}
    QDataSignalAccess(QData &raw) : raw(addressof(raw)){}
    uint64_t getU64() {return *raw;}
    uint64_t getU64_mem(size_t index) {return raw[index];}
    void setU64(uint64_t value)  {*raw = value; }
    void setU64_mem(uint64_t value, size_t index){raw[index] = value; }
};

class  WDataSignalAccess : public ISignalAccess{
public:
    WData *raw;
    uint32_t width;
    uint32_t wordsCount;
    bool sint;

    WDataSignalAccess(WData *raw, uint32_t width, bool sint) :
      raw(raw), width(width), wordsCount((width+31)/32), sint(sint) {}

    uint64_t getU64_mem(size_t index) {
      WData *mem_el = &(raw[index*wordsCount]);
      return mem_el[0] + (((uint64_t)mem_el[1]) << 32);
    }

    uint64_t getU64() { return getU64_mem(0); }

    void setU64_mem(uint64_t value, size_t index)  {
      WData *mem_el = &(raw[index*wordsCount]);
      mem_el[0] = value;
      mem_el[1] = value >> 32;
      uint32_t padding = ((value & 0x8000000000000000l) && sint) ? 0xFFFFFFFF : 0;
      for(uint32_t idx = 2;idx < wordsCount;idx++){
        mem_el[idx] = padding;
      }

      if(width%32 != 0) mem_el[wordsCount-1] &= (1l << width%32)-1;
    }

    void setU64(uint64_t value)  {
      setU64_mem(value, 0);
    }

    void getAU8_mem(JNIEnv *env, jbyteArray value, size_t index) {
      WData *mem_el = &(raw[index*wordsCount]);
      uint32_t byteCount = wordsCount*4;
      uint32_t shift = 32-(width % 32);
      uint32_t backup = mem_el[wordsCount-1];
      uint8_t values[byteCount + !sint];
      if(sint && shift != 32) mem_el[wordsCount-1] = (((int32_t)backup) << shift) >> shift;
      for(uint32_t idx = 0;idx < byteCount;idx++){
        values[idx + !sint] = ((uint8_t*)mem_el)[byteCount-idx-1];
      }
      (env)->SetByteArrayRegion ( value, 0, byteCount + !sint, reinterpret_cast<jbyte*>(values));
      mem_el[wordsCount-1] = backup;
    }

    void getAU8(JNIEnv *env, jbyteArray value) {
      getAU8_mem(env, value, 0);
    }

    void setAU8_mem(JNIEnv *env, jbyteArray jvalue, int length, size_t index) {
      WData *mem_el = &(raw[index*wordsCount]);
      jbyte value[length];
      (env)->GetByteArrayRegion( jvalue, 0, length, value);
      uint32_t padding = (value[0] & 0x80 && sint) != 0 ? 0xFFFFFFFF : 0;
      for(uint32_t idx = 0;idx < wordsCount;idx++){
        mem_el[idx] = padding;
      }
      uint32_t capedLength = length > 4*wordsCount ? 4*wordsCount : length;
      for(uint32_t idx = 0;idx < capedLength;idx++){
        ((uint8_t*)mem_el)[idx] = value[length-idx-1];
      }
      if(width%32 != 0) mem_el[wordsCount-1] &= (1l << width%32)-1;
    }

    void setAU8(JNIEnv *env, jbyteArray jvalue, int length) {
      setAU8_mem(env, jvalue, length, 0);
    }
};

class Wrapper_${uniqueId};
thread_local Wrapper_${uniqueId} *simHandle${uniqueId};

#include <chrono>
using namespace std::chrono;

class Wrapper_${uniqueId}{
public:
    uint64_t time;
    high_resolution_clock::time_point lastFlushAt;
    uint32_t timeCheck;
    bool waveEnabled;
    V${config.toplevelName} top;
    ISignalAccess *signalAccess[${config.signals.length}];
    #ifdef TRACE
	  Verilated${format.ext.capitalize}C tfp;
	  #endif
    string name;

    Wrapper_${uniqueId}(const char * name){
      simHandle${uniqueId} = this;
      time = 0;
      timeCheck = 0;
      lastFlushAt = high_resolution_clock::now();
      waveEnabled = true;
${
          val signalInits = for ((signal, id) <- config.signals.zipWithIndex) yield {
            val typePrefix = if (signal.dataType.width <= 8) "CData"
            else if (signal.dataType.width <= 16) "SData"
            else if (signal.dataType.width <= 32) "IData"
            else if (signal.dataType.width <= 64) "QData"
            else "WData"
            val enforcedCast = if (signal.dataType.width > 64) "(WData*)" else ""
            val signalReference = s"top.${signal.path.mkString("->")}"
            val memPatch = if (signal.dataType.isMem) "[0]" else ""

            s"      signalAccess[$id] = new ${typePrefix}SignalAccess($enforcedCast $signalReference$memPatch ${if (signal.dataType.width > 64) s" , ${signal.dataType.width}, ${if (signal.dataType.isInstanceOf[SIntDataType]) "true" else "false"}" else ""});\n"

          }

          signalInits.mkString("")
        }
      #ifdef TRACE
      Verilated::traceEverOn(true);
      top.trace(&tfp, 99);
      tfp.open((std::string("${new File(config.vcdPath).getAbsolutePath.replace("\\", "\\\\")}/${if (config.vcdPrefix != null) config.vcdPrefix + "_" else ""}") + name + ".${format.ext}").c_str());
      #endif
      this->name = name;
    }

    virtual ~Wrapper_${uniqueId}(){
      for(int idx = 0;idx < ${config.signals.length};idx++){
          delete signalAccess[idx];
      }

      #ifdef TRACE
      if(waveEnabled) tfp.dump((vluint64_t)time);
      tfp.close();
      #endif
      #ifdef COVERAGE
      VerilatedCov::write((("${new File(config.vcdPath).getAbsolutePath.replace("\\", "\\\\")}/${if (config.vcdPrefix != null) config.vcdPrefix + "_" else ""}") + name + ".dat").c_str());
      #endif
    }

};

double sc_time_stamp () {
  return simHandle${uniqueId}->time;
}


#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <stdint.h>

#define API __attribute__((visibility("default")))


JNIEXPORT Wrapper_${uniqueId} * API JNICALL ${jniPrefix}newHandle_1${uniqueId}
  (JNIEnv * env, jobject obj, jstring name, jint seedValue){
    #if defined(_WIN32) && !defined(__CYGWIN__)
    srand(seedValue);
    #else
    srand48(seedValue);
    #endif
    Verilated::randReset(2);
    const char* ch = env->GetStringUTFChars(name, 0);
    Wrapper_${uniqueId} *handle = new Wrapper_${uniqueId}(ch);
    env->ReleaseStringUTFChars(name, ch);
    return handle;
}

JNIEXPORT jboolean API JNICALL ${jniPrefix}eval_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} *handle){
   handle->top.eval();
   return Verilated::gotFinish();
}


JNIEXPORT void API JNICALL ${jniPrefix}sleep_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} *handle, uint64_t cycles){
  #ifdef TRACE
  if(handle->waveEnabled) {
    handle->tfp.dump((vluint64_t)handle->time);
  }
  handle->timeCheck++;
  if(handle->timeCheck > 10000){
    handle->timeCheck = 0;
    high_resolution_clock::time_point timeNow = high_resolution_clock::now();
    duration<double, std::milli> time_span = timeNow - handle->lastFlushAt;
    if(time_span.count() > 1e3){
      handle->lastFlushAt = timeNow;
      handle->tfp.flush();
    }
  }
  #endif
  handle->time += cycles;
}

JNIEXPORT jlong API JNICALL ${jniPrefix}getU64_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} *handle, int id){
  return handle->signalAccess[id]->getU64();
}

JNIEXPORT jlong API JNICALL ${jniPrefix}getU64mem_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} *handle, int id, uint64_t index){
  return handle->signalAccess[id]->getU64_mem(index);
}

JNIEXPORT void API JNICALL ${jniPrefix}setU64_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} *handle, int id, uint64_t value){
  handle->signalAccess[id]->setU64(value);
}

JNIEXPORT void API JNICALL ${jniPrefix}setU64mem_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} *handle, int id, uint64_t value, uint64_t index){
  handle->signalAccess[id]->setU64_mem(value, index);
}

JNIEXPORT void API JNICALL ${jniPrefix}deleteHandle_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} * handle){
  delete handle;
}

JNIEXPORT void API JNICALL ${jniPrefix}getAU8_1${uniqueId}
  (JNIEnv * env, jobject obj, Wrapper_${uniqueId} * handle, jint id, jbyteArray value){
  handle->signalAccess[id]->getAU8(env, value);
}

JNIEXPORT void API JNICALL ${jniPrefix}getAU8mem_1${uniqueId}
  (JNIEnv * env, jobject obj, Wrapper_${uniqueId} * handle, jint id, jbyteArray value, uint64_t index){
  handle->signalAccess[id]->getAU8_mem(env, value, index);
}

JNIEXPORT void API JNICALL ${jniPrefix}setAU8_1${uniqueId}
  (JNIEnv * env, jobject obj, Wrapper_${uniqueId} * handle, jint id, jbyteArray value, jint length){
  handle->signalAccess[id]->setAU8(env, value, length);
}

JNIEXPORT void API JNICALL ${jniPrefix}setAU8mem_1${uniqueId}
  (JNIEnv * env, jobject obj, Wrapper_${uniqueId} * handle, jint id, jbyteArray value, jint length, uint64_t index){
  handle->signalAccess[id]->setAU8_mem(env, value, length, index);
}

JNIEXPORT void API JNICALL ${jniPrefix}enableWave_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} * handle){
  handle->waveEnabled = true;
}

JNIEXPORT void API JNICALL ${jniPrefix}disableWave_1${uniqueId}
  (JNIEnv *, jobject, Wrapper_${uniqueId} * handle){
  handle->waveEnabled = false;
}

#ifdef __cplusplus
}
#endif
     """
      val outFile = new java.io.FileWriter(wrapperCppPath)
      outFile.write(wrapperString)
      outFile.flush()
      outFile.close()

      val exportMapString =
        s"""CODEABI_1.0 {
           |    global: $jniPrefix*;
           |    local: *;
           |};""".stripMargin

      val exportmapFile = new java.io.FileWriter(s"${workspacePath}/${workspaceName}/libcode.version")
      exportmapFile.write(exportMapString)
      exportmapFile.flush()
      exportmapFile.close()
    }

    class Logger extends ProcessLogger {
      override def err(s: => String): Unit = {
        if (!s.startsWith("ar: creating ")) println(s)
      }

      override def out(s: => String): Unit = {}

      override def buffer[T](f: => T) = f
    }

    //     VL_THREADED
    def compileVerilator(): Unit = {
      val java_home = System.getProperty("java.home")
      assert(java_home != "" && java_home != null, "JAVA_HOME need to be set")
      val jdk = java_home.replace("/jre", "").replace("\\jre", "")
      val jdkIncludes = if (isWindows) {
        new File(s"${workspacePath}\\${workspaceName}").mkdirs()
        FileUtils.copyDirectory(new File(s"$jdk\\include"), new File(s"${workspacePath}\\${workspaceName}\\jniIncludes"))
        s"jniIncludes"
      } else {
        jdk + "/include"
      }

      val arch = System.getProperty("os.arch")
      val flags = if (isMac) List("-dynamiclib") else (if (arch == "arm" || arch == "aarch64") List("-fPIC", "-shared", "-Wno-attributes") else List("-fPIC", "-m64", "-shared", "-Wno-attributes"))

      config.rtlSourcesPaths.filter(s => s.endsWith(".bin") || s.endsWith(".mem")).foreach(path => FileUtils.copyFileToDirectory(new File(path), new File(s"./")))

      //    --output-split-cfuncs 200
      //    --output-split-ctrace 200

      val waveArgs = format match {
        case WaveFormat.FST => "-CFLAGS -DTRACE --trace-fst"
        case WaveFormat.VCD => "-CFLAGS -DTRACE --trace"
        case WaveFormat.NONE => ""
      }

      val covArgs = config.withCoverage match {
        case true => "-CFLAGS -DCOVERAGE --coverage"
        case false => ""
      }

      val rtlIncludeDirsArgs = config.rtlIncludeDirs.map(e => s"-I${new File(e).getAbsolutePath}").mkString(" ")


      val verilatorScript =
        s""" set -e ;
           | ${if (isWindows) "verilator_bin.exe" else "verilator"}
           | ${flags.map("-CFLAGS " + _).mkString(" ")}
           | ${flags.map("-LDFLAGS " + _).mkString(" ")}
           | -CFLAGS -I"$jdkIncludes" -CFLAGS -I"$jdkIncludes/${if (isWindows) "win32" else (if (isMac) "darwin" else "linux")}"
           | -CFLAGS -fvisibility=hidden
           | -LDFLAGS -fvisibility=hidden
           | -CFLAGS -std=c++11
           | -LDFLAGS -std=c++11
           | --autoflush
           | --output-split 50000
           | --output-split-cfuncs 5000
           | --output-split-ctrace 5000
           | -Wno-WIDTH -Wno-UNOPTFLAT -Wno-CMPCONST -Wno-UNSIGNED
           | --x-assign unique
           | --trace-depth ${config.waveDepth}
           | -O3
           | -CFLAGS -O${config.optimisationLevel}
           | $waveArgs
           | $covArgs
           | --Mdir ${workspaceName}
           | --top-module ${config.toplevelName}
           | $rtlIncludeDirsArgs
           | -cc ${
          config.rtlSourcesPaths.filter(e => e.endsWith(".v") ||
            e.endsWith(".sv") ||
            e.endsWith(".h"))
            .map(new File(_).getAbsolutePath)
            .map('"' + _.replace("\\", "/") + '"')
            .mkString(" ")
        }
           | --exe $workspaceName/$wrapperCppName
           | ${config.simulatorFlags.mkString(" ")}""".stripMargin.replace("\n", "")

      val verilatorScriptFile = new PrintWriter(new File(workspacePath + "/verilatorScript.sh"))
      verilatorScriptFile.write(verilatorScript)
      verilatorScriptFile.close

      val shCommand = if (isWindows) "sh.exe" else "sh"
      println("[Progress] Start running verilatorScript.sh")
      assert(Process(Seq(shCommand, "verilatorScript.sh"),
        new File(workspacePath)).!(new Logger()) == 0, "Verilator invocation failed")

      genWrapperCpp()
      val threadCount = if (isWindows || isMac) Runtime.getRuntime().availableProcessors() else VanillaCpuLayout.fromCpuInfo().cpus()
      assert(s"make -j$threadCount VM_PARALLEL_BUILDS=1 -C ${workspacePath}/${workspaceName} -f V${config.toplevelName}.mk V${config.toplevelName} CURDIR=${workspacePath}/${workspaceName}".!(new Logger()) == 0, "Verilator C++ model compilation failed")

      FileUtils.copyFile(new File(s"${workspacePath}/${workspaceName}/V${config.toplevelName}${if (isWindows) ".exe" else ""}"), new File(s"${workspacePath}/${workspaceName}/${workspaceName}_$uniqueId.${if (isWindows) "dll" else (if (isMac) "dylib" else "so")}"))
    }

    def compileJava(): Unit = {
      val verilatorNativeImplCode =
        s"""package wrapper_${workspaceName};
           |import spinal.sim.IVerilatorNative;
           |
           |public class VerilatorNative implements IVerilatorNative {
           |    public long newHandle(String name, int seed) { return newHandle_${uniqueId}(name, seed);}
           |    public boolean eval(long handle) { return eval_${uniqueId}(handle);}
           |    public void sleep(long handle, long cycles) { sleep_${uniqueId}(handle, cycles);}
           |    public long getU64(long handle, int id) { return getU64_${uniqueId}(handle, id);}
           |    public long getU64_mem(long handle, int id, long index) { return getU64mem_${uniqueId}(handle, id, index);}
           |    public void setU64(long handle, int id, long value) { setU64_${uniqueId}(handle, id, value);}
           |    public void setU64_mem(long handle, int id, long value, long index) { setU64mem_${uniqueId}(handle, id, value, index);}
           |    public void getAU8(long handle, int id, byte[] value) { getAU8_${uniqueId}(handle, id, value);}
           |    public void getAU8_mem(long handle, int id, byte[] value, long index) { getAU8mem_${uniqueId}(handle, id, value, index);}
           |    public void setAU8(long handle, int id, byte[] value, int length) { setAU8_${uniqueId}(handle, id, value, length);}
           |    public void setAU8_mem(long handle, int id, byte[] value, int length, long index) { setAU8mem_${uniqueId}(handle, id, value, length, index);}
           |    public void deleteHandle(long handle) { deleteHandle_${uniqueId}(handle);}
           |    public void enableWave(long handle) { enableWave_${uniqueId}(handle);}
           |    public void disableWave(long handle) { disableWave_${uniqueId}(handle);}
           |
           |
           |    public native long newHandle_${uniqueId}(String name, int seed);
           |    public native boolean eval_${uniqueId}(long handle);
           |    public native void sleep_${uniqueId}(long handle, long cycles);
           |    public native long getU64_${uniqueId}(long handle, int id);
           |    public native long getU64mem_${uniqueId}(long handle, int id, long index);
           |    public native void setU64_${uniqueId}(long handle, int id, long value);
           |    public native void setU64mem_${uniqueId}(long handle, int id, long value, long index);
           |    public native void getAU8_${uniqueId}(long handle, int id, byte[] value);
           |    public native void getAU8mem_${uniqueId}(long handle, int id, byte[] value, long index);
           |    public native void setAU8_${uniqueId}(long handle, int id, byte[] value, int length);
           |    public native void setAU8mem_${uniqueId}(long handle, int id, byte[] value, int length, long index);
           |    public native void deleteHandle_${uniqueId}(long handle);
           |    public native void enableWave_${uniqueId}(long handle);
           |    public native void disableWave_${uniqueId}(long handle);
           |
           |    static{
           |      System.load("${new File(s"${workspacePath}/${workspaceName}").getAbsolutePath.replace("\\", "\\\\")}/${workspaceName}_$uniqueId.${if (isWindows) "dll" else (if (isMac) "dylib" else "so")}");
           |    }
           |}
       """.stripMargin

      val verilatorNativeImplFile = new DynamicCompiler.InMemoryJavaFileObject(s"wrapper_${workspaceName}.VerilatorNative", verilatorNativeImplCode)
      import collection.JavaConverters._
      DynamicCompiler.compile(List[JavaFileObject](verilatorNativeImplFile).asJava, s"${workspacePath}/${workspaceName}")
    }

    def checks(): Unit = {
      if (System.getProperty("java.class.path").contains("sbt-launch.jar")) {
        System.err.println("""[Error] It look like you are running the simulation with SBT without having the SBT 'fork := true' configuration.\n  Add it in the build.sbt file to fix this issue, see https://github.com/SpinalHDL/SpinalTemplateSbt/blob/master/build.sbt""")
        throw new Exception()
      }
    }

    def showTime() = println(s"[Time] ${System.nanoTime() * 1E-9}")

    clean()
    checks()
    println(s"[Progress] Running verilator Script.sh")
    compileVerilator()
    showTime()
    compileJava()
    showTime()

    val nativeImpl = DynamicCompiler.getClass(s"wrapper_${workspaceName}.VerilatorNative", s"${workspacePath}/${workspaceName}")
    showTime()
    val nativeInstance: IVerilatorNative = nativeImpl.newInstance().asInstanceOf[IVerilatorNative]
    showTime()

    def instanciate(name: String, seed: Int) = nativeInstance.newHandle(name, seed)

    override def isBufferedWrite: Boolean = false
  }

  class MySimVerilator(backend: MyVerilatorBackend,
                       handle: Long) extends SimRaw() {

    override def getIntMem(signal: Signal,
                           index: Long): Int = {
      assert(signal.id != -1, "You can't access this signal in the simulation, as it isn't public")
      signal.dataType.raw64ToInt(backend.nativeInstance.getU64_mem(handle,
        signal.id,
        index), signal: Signal)
    }

    def setIntMem(signal: Signal,
                  value: Int,
                  index: Long): Unit = {
      setLongMem(signal, value, index)
    }

    override def getLongMem(signal: Signal,
                            index: Long): Long = {
      assert(signal.id != -1, "You can't access this signal in the simulation, as it isn't public")
      signal.dataType.raw64ToLong(backend.nativeInstance.getU64_mem(handle,
        signal.id,
        index), signal: Signal)
    }

    override def setLongMem(signal: Signal,
                            value: Long,
                            index: Long): Unit = {
      assert(signal.id != -1, "You can't access this signal in the simulation, as it isn't public")
      backend.nativeInstance.setU64_mem(handle,
        signal.id,
        signal.dataType.longToRaw64(value, signal: Signal),
        index)
    }

    override def getBigIntMem(signal: Signal,
                              index: Long) = {
      if (signal.dataType.width < 64 || (signal.dataType.width == 64 && signal.dataType.isInstanceOf[SIntDataType])) {
        getLongMem(signal, index)
      } else if (signal.dataType.width == 64) {
        val rawValue = backend.nativeInstance.getU64_mem(handle,
          signal.id,
          index)
        if (rawValue >= 0) {
          BigInt(rawValue)
        } else {
          BigInt(rawValue + 1) + SimVerilator.bigInt32b
        }
      } else {
        if (signal.dataType.isInstanceOf[SIntDataType]) {
          val array = new Array[Byte]((signal.dataType.width + 31) / 32 * 4)
          backend.nativeInstance.getAU8_mem(handle,
            signal.id,
            array,
            index)
          BigInt(array)
        } else {
          val array = new Array[Byte]((signal.dataType.width + 31) / 32 * 4 + 1)
          backend.nativeInstance.getAU8_mem(handle,
            signal.id,
            array,
            index)
          array(0) = 0
          BigInt(array)
        }
      }
    }

    override def setBigIntMem(signal: Signal,
                              value: BigInt,
                              index: Long): Unit = {
      val valueBitLength = value.bitLength + (if (value.signum == -1) 1 else 0)
      if (valueBitLength <= 63) {
        setLongMem(signal,
          value.toLong,
          index)
      } else if (valueBitLength == 64 && signal.dataType.width == 64) {
        assert(signal.id != -1, "You can't access this signal in the simulation, as it isn't public")
        val valueLong = value.toLong
        signal.dataType.checkIs64(valueLong, signal: Signal)
        backend.nativeInstance.setU64_mem(handle, signal.id, valueLong, index)
      } else {
        signal.dataType.checkBigIntRange(value, signal)
        val array = value.toByteArray
        backend.nativeInstance.setAU8_mem(handle,
          signal.id,
          array,
          array.length,
          index)
      }
    }

    override def getInt(signal: Signal): Int = {
      getIntMem(signal, 0)
    }

    def setInt(signal: Signal, value: Int) {
      setLongMem(signal, value, 0)
    }

    override def getLong(signal: Signal): Long = {
      getLongMem(signal, 0)
    }

    override def setLong(signal: Signal, value: Long) {
      setLongMem(signal, value, 0)
    }

    override def getBigInt(signal: Signal): BigInt = {
      getBigIntMem(signal, 0)
    }

    override def setBigInt(signal: Signal, value: BigInt) {
      setBigIntMem(signal, value, 0)
    }

    override def eval(): Boolean = backend.nativeInstance.eval(handle)

    override def sleep(cycles: Long) = backend.nativeInstance.sleep(handle, cycles)

    override def end() = backend.nativeInstance.deleteHandle(handle)

    override def isBufferedWrite: Boolean = false

    override def enableWave(): Unit = backend.nativeInstance.enableWave(handle)

    override def disableWave(): Unit = backend.nativeInstance.disableWave(handle)
  }
}
