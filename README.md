[2021秋季 数字设计研讨会](https://nonchalant-frill-fc5.notion.site/2021-f22e01ec73b641d19863118faf349647)

# Chainsaw - 以DFG为核心的DSP DSL

### 近期常用链接

[DFG模型](https://www.notion.so/DFG-fcad00f0a9164fb79cca8049caa4c343)

# 什么是Chainsaw?

## 效果展示

- 原始设计图(FIR框图)

- 图设计语法

```scala
val myFIR = DFGGraph()
val adder = (dataIns: Seq[FIX]) => Seq(dataIns(0) + dataIns(1)).asDSPNode
val cmult = (dataIns: Seq[FIX], constant:Int) => Seq(dataIns(0) * constant).asDSPNode
val Seq(a0,a1,a2) = ...
val Seq(m0,m1,m2,m3) = ...
val x = myFIR.addInput()
myFIR.addPath(x >> m0 >> 1 >> a0 >> a2 >> a3 >> )
...
myFIR.setOutput(a3)

myFIR.fold(2)
GenRTL(myFIR)
VivadoSynth(myFIR)
```

- DSL语法

```scala
val dfg = "y = 1 * x@3 + 2 * x@2 + 3 * x@1 + 4 * x".asDFG.fold(2)

GenRTL(myFIR)
VivadoSynth(myFIR)
```

- IP语法

```scala
val myFIR = FIR(1,2,3,4, folding = 2, arch = SYSTOLIC)

GenRTL(myFIR)
VivadoSynth(myFIR)
```

## Chainsaw的机制

- 设计的底层要素 - 面向硬件算法/架构研究者,在图级别工作

- 设计的中层要素 - 面向DSP系统设计者,在DSL级别工作

- 设计的高层要素 - 面向调包者,在IP级别工作

- 设计的辅助要素
- [ ]  补充图示: dive into Chainsaw
- Chainsaw Mechanisms详解页面

  [Chainsaw Mechanisms](https://www.notion.so/d66d9d6ef1f542bf9e35e5408351a363)


[为实现Chainsaw对SpinalHDL的研究](https://www.notion.so/Chainsaw-SpinalHDL-addf6d7c5fd441c8bf8f1a349b648eba)

## Chainsaw的IP

- Chainsaw IPs详解页面

  [Chainsaw IPs](https://www.notion.so/e3a31d826f1f4d95aae0f22dc1ab5258)


---

IP的package划分和参考书

- communication.channelCoding - 信道编码和解码 - CodingTheory

# 同类工作对比

## 开源社区/学术界工作

- Chisel-DSP
    - [https://github.com/ucb-bar/dsptools](https://github.com/ucb-bar/dsptools)
    - 提供了框架,但没有提供设计
    - 许多想法和我一致
    - 一段时间没有更新了,我可以做得更好

- FloPoCo
    - 提出了很多有趣的想法,但(一定程度上,受限于当时的技术)实现手段的自动化不足
    - 有较多的built-in design
    - 可以借鉴其设计理念,一对一cover
    - 可以借鉴其对于设计的分级和分类
    - 可以调用/移植其现有设计

- Spiral
    - DARPA资助,CMU维护的数值计算软/硬件生成器
    - 还没有仔细了解,但他们的研究重点似乎是数值计算本身
    - 虽然有几个令人印象深刻的在线verilog生成器,但总的来说,这个项目并不是面向RTL设计的
    - 与FloPoCo同样地,也许可以使用这个项目作为我的后端
    - 实际上,$H_\mathrm{cub}$算法的发明者是这个项目的领导者之一

## 商业工具

- Xilinx LogiCore IP
    - building blocks
        - complex multiplier
        - CORDIC
    - filters
        - CIC Compiler
        - FIR Compiler
        - DUC/DDC
    - modulation
        - DDS Compiler
    - transforms
        - DFT
        - FFT

- System Generator



- Matlab HDL Coder

- Intel DSP Designer

[以上工具的demos](https://www.notion.so/demos-673d53f1d2b64512866f6f6e5c61ccef)

## INNOVATIONS - 与上面工作相比

- innovations
    - just-right: more than philosophy
        - 通过传播范围的类型系统实现真正的自动just-right设计
    - verification: verifying on the fly
        - verification
            - 利用SpinalHDL的仿真接口,真正实现一站式仿真
            - 支持直接的数值输入和输出
            - 支持基于范围生成遍历testCases和随机testCases
            - 标准testbench模板,直接利用时序信息
    - design and reuse: hierarchical & no more black box
        - 不需要生成不可读的设计再使用
        - 真正可读可写的codegen(HGL,而非templated-HDL)
        - 超强的参数化
        - 与高级语言的兼容性
        - 高度去耦合 → 自动每个部件的改善获得收益
    - 下面的内容在第一期之后实现
        - visualization: submit your paper the day after tomorrow
            - 利用tag系统,生成计算流图
                - 运算tag成为相应符号
                - 并且,使用时序断点tag来进行对齐

---

# 论文准备

## 论文大纲

- 大纲
    1. 摘要
        - 三个核心特性
        - 所有亮点
            - 第一个RTL级的,基于HGL的DSP DSL
            - 使得高级架构变换变得前所未有的简单
            - 使得精度和位宽的specification变得前所未有的简单
            - 软硬协同验证
            - 现成的全开源工具链
            - 丰富的可扩展性,未来可期
    2. 历史回顾
        - 之前的此类设计
            - 为什么DSL,而不是C/python/java?
                - 我们应当帮助硬件工程师,而不是试图让软件工程师取而代之
                - 我们认为上面的目标永远不可能达到,其结果往往是让软件和硬件工程师都付出了更多的学习成本
            - 为什么是scala-based DSL,而不是全新的DSL
                - scala作为DSL宿主语言的优势
                - Chisel和SpinalHDL的成功
                - scala与其它重要库的互操作性
                    - matlab JAVA API
                    - JGraphT
                    - Spire
        - 我们的设计哲学
            - 充分但最简的信息
                - "充分" - bit-accurate,而不是高层次综合
                - "最简" - 通过领域特定知识,将语言的形式变为最简
                    - 如果你不同意领域特定知识,你始终可以在最细的颗粒度上操作
                - 一个例子: FIR的公式和精度,已经足够,为什么要写那么多代码?
            - 拒绝贪心
                - 拒绝实现复杂控制逻辑 - 交给SpinalHDL / verilog
                - 拒绝实现高层次综合
                    - 只做有限的DFG变换
                    - 并且,通过一种格式为高层次综合提供可能性
                - 通过复用来弥补自己的不足,而不是实现一切
                    - 并且,这样带来了"信息保持"特性
            - 多范式,自助餐式的设计 - 通过开源,这是可能的
                - 研究者可以深入图框架和类型框架
                - 设计者可以使用DSL
                - 需要IP的人可以仅仅运行一段代码
    3. motivational example: 与silage的比较 - 也许换成FIR/FFT更好?
        1. 图1,FIR的结构,silage代码,和三个层次上的Chainsaw代码
        2. 图2,FFT是怎么通过retiming,folding,unfolding成为一个family
    - Chainsaw描述

![Untitled](Chainsaw%20-%20%E4%BB%A5DFG%E4%B8%BA%E6%A0%B8%E5%BF%83%E7%9A%84DSP%20DSL%20d99bf4c9f5334923956f494843e1a207/Untitled.png)

硬件架构

![Untitled](Chainsaw%20-%20%E4%BB%A5DFG%E4%B8%BA%E6%A0%B8%E5%BF%83%E7%9A%84DSP%20DSL%20d99bf4c9f5334923956f494843e1a207/Untitled%201.png)

silage描述

```scala
import Chainsaw._

val word = HardType(Fix(20,8))
val coef = HardType(Fix(10,4))

val adaptor = Seq("b + (b - a) * g", "a + (b - a) * g").asDSPNode
	.setType(Seq(word, word, coef))
val post = "(b - a) * 0.5".asDSPNode

val whole = new DFGArea{ // DFGArea中有一个隐式的DFG
	val in = input()
	val ada0, ada1, ada2 = adaptor()
	val post0 = post()
	ada0(in, ada1(0), 0.1)
	ada1(ada0(1)@1, ada1(1)@1, 0.001)
	ada2(in, ada2(1)@1, 0.001)
}

GenRTL(whole)
VivadoSynth(whole)

```

1. 特性说明,沿着motivational example展开
    1. DFG特性和原理
    2. 数值类型特性和原理
    3. DSL形式和设计范围(对照silage的内容)
        1. 对于flow和delay的描述
        2. 对于control的描述
        3. 对于精度类型的描述
        4. 可扩展性和兼容性
2. 总结和展望
    1. 丰富IP库
    2. 增强向量化
    3. 增加对多速率系统的支持

[关于类型推断的论文大纲](https://www.notion.so/0882840386604f0cb7009f36519075ae)

## 文献调研

- 重要文章

  [数字设计研讨会 - 文献数据库](https://www.notion.so/4e4d785fcb4a441eb47234602b0dd37a)

- 重要人物

  [DSP研究的重要人物](https://www.notion.so/101b9c8b65f7473fb545ca9612d718d8)

- 类似工作

  [Framework Comparing（按发表年份升序排列）](https://www.notion.so/2965dd35c4854a2b868e9ec881e2f3f6)


@Ricardo Lee 这个可能要你来定一下，我这里大概写了一点：

- 框架和框架之间比较的 Attributes：
    - HGL
    - 开源
    - 参数化定制化能力
    - 允许的架构的变换能力
        - Pipelining
        - Retiming
        - Folding
        - Unfolding
        - Systolic Array
    - 设计空间探索
    - 可扩展性
    - 结构与算子的强解耦性
    - 全平台能力
    - 快速迭代能力
    - ...

- 精度推断、精度传播、误差分析

- 一些体会：
    - 语言丰富多样，SystemC、matlab、HLS、Silage、CAL、... 等等
    - 设计空间探索对于这样的一个设计框架基本上是属于“标配功能”
    - 快速迭代能力也是这些框架大部分所声称的卖点
    - 为了适配现有前后端流程，生成可综合RTL是较为普遍的策略。当然也有直接生成layout的（少）
    - 较弱的可扩展性是大部分框架的通病
        - 假如框架是开源的
            - 你需要彻底理解它繁复庞杂的整个大系统，然后才能在此之上再去做设计改动，但是会牵一发动全身
            - 也许框架本身就没想着给普通设计者去使用


---

日程和分工

[Chainsaw开发日程](https://www.notion.so/cf61a0d1e7b142b28163c7c89fd6d24c)

- 整理和计划工作
    - [x]  整合所有关于Chainsaw的notion页面,给出一个可读的主页
    - [x]  将Chainsaw整理到其它人能够协助我开发的程度,给德昊和泳豪(甚至,所有人)开放push权限,增加一个代码备份点
    - [ ]  实现DFG和硬件类型的分离
    - [ ]  整理Real代码
- DFG模型的基础性工作
    - [ ]  实现数值类型和数值模型的分离
    - [ ]  读论文确认DFG模型所需要的所有基本要素
    - [ ]  设定DFG所有变换的API
    - [ ]  编写DFG设计所需要的回归测试
- 文献阅读工作
    - [ ]  最优先阅读的文献
        - [ ]  cathedral
        - [ ]  架构变换综述
        - [ ]  silage

## 分工

- 李天瑞
    - 核心部分的设计,主要是
        - DFG模型
        - 数据类型设计
        - 基本operator的硬件实现
            - MUX
            - DELAY
            - 各种乘法和加法

- 陈泳豪
    - 调查学术界/开源社区的DSP设计框架
    - 可视化框架的设计
    - Vivado交互框架的设计
    - 编写已经实现的Vivadoflow的文档

- 向德昊
    - 调查商业DSP设计工具
    - 调查maven发布方法
    - Chainsaw的测试框架和协议设计
        - 编写已经实现的测试方法的文档

          [笔记](https://www.notion.so/af541256a0014f4b870d283672f198a6)

        - 编写封装后的matlab scala API的文档

- 李权鑫: 代码测试,论文阅读
    - 从文献和教材中向Chainsaw添加回归测试
    - 整理先前的算法内容的文档,和API文档

- 可能的其它人
    - 研究和清理之前实现的IP

## 干扰因素

- FTN设计的推进
- 研讨会内容的准备

---

# Chainsaw Doc

- 页面上的Doc只负责下面内容:
    - 基本机制的简单介绍
    - 整个名称空间的设计
        - Chainsaw.package.scala
            - 和Real有关的代码
                - 暂略
            - 简化simulation的Util
                - SFix类型的pimper → `SimSFixPimper`
                - ComplexNumber的Pimper → `SimComplexPimper` 其内容是基于SFix的
                - 各种println方法和作为println开关的`printlnWhenDebug`
                - 作为bit sequence的BigInt → `BigIntUtil`
                - 作为bit sequence的String → `BinaryStringUtil`
                - Seq的Util
                    - 将Seq[T <: Data]视为Vec的隐式转换
            - 对定点类型的增强
                - 为SFix增加了unary -,有进位加法和减法
                -
            - 对随机数的增强
                - `nextComplex` - 产生一个随机的复数,范围同nextDouble
                - `nextBigInt` - 产生一个随机的,任意长度的BigInt,是正数,用于发生比特序列
                - `nextBinaryString` - 产生String,目的同上
                - 另一个直接地,在仿真时发生随机序列的方法是直接对信号进行randomize,然后采集它们作为testCase
            - 快速生成代码/仿真/synth/impl
                - `GenRTL`
                - Vivado系列
                    - `VivadoSynth`
                    - `VivadoImpl`

          以Seq为核心的"有序集合"类型

          重新梳理Real的import逻辑

        - core -providing core mechanisms for all the designs, including design, high-level transformation, numeric data type, and data-driven test framework
            - dfg - dataflow graph model
            - archs - DFG generators(thus, they represent specific "architecture")
            - dsptest -  providing test framework for data-driven module(DFG)
            - numeric - numeric data type which can be easily peek/poke, and is self-validated
            - dsl - interface & utils helping users to use mechanisms above fluently
        - lib - providing off-the-shelf designs and utils
            - comparith - building blocks which you may find in *Computer Arithmetic*
            - dsp - transformations on real/complex field, which you may find in DSP with FPGA, fast algo
            - comm - designs for communications, especially for encoding/decoding
        - Transform - Chainsaw所采用的协议和测试规范
        - Crypto - 密码学相关的模块
        - Xilinx - 和Xilinx器件及工具链相关的内容
        - Examples - 存放一些示例代码,可能是关于某个语法如何使用,或是某个第三方库的示例
- 对于更加详细的doc
    - [ ]  调查scaladoc的网页生成,制定doc要求
- 对于IP的doc,遵守IP文档规范,在notion上给出文档

---

[Chainsaw依赖的库](https://www.notion.so/d8e072c734af40449113b37368565bee)

[我的SpinalHDL案例](https://www.notion.so/SpinalHDL-0b32d0349ff941879c4b871dd7ca6db4)

两张表

- 抽象代数性质表
- DSP设计与其数学本质,加速方法的映射表

debug: 细节

info: 日常/流程 - 指明当前的阶段

warn: 不违法,但很可能有错

error: