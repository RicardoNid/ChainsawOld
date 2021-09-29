# Chainsaw - an open source, just-right arithmetic library for FPGA/ASIC based on SpinalHDL

[quicknote on chainsaw](https://www.notion.so/quicknote-on-chainsaw-eb72b1d47c4b4e1c8b1088339fe39732)

[Chainsaw标准开发流程 - 以CORDIC为例](https://www.notion.so/Chainsaw-CORDIC-3795177f853f4592a277e6ce952377a5)

# contents & innovations

- contents
    - 第一阶段特性
        - 一套自动测试模板
            - 为测试添加协议
            - 一个完备的数值信号类型
                - 带有可传播的范围
                - 与数值运算联动
                - 更良好的仿真时输入/输出方法
                - 更准确的rounding/overflow策略
            - 对计算流图进行测试
        - 一个位宽推断,just right design框架
            - 基于范围传播
            - 辅以一个graph-based backend
        - 对于常见structure,而不只是design的抽象
    - 下面的特性在第一期之后实现
        - 一个面向模运算的信号类型
        - 一系列常用DSP设计(IPs, and more than IPs)
            - 覆盖Xilinx IP
            - 复用/移植FloPoCo
            - 实现DSP with FPGA
            - 模仿BLAS,对算子分级
        - semi-auto pipelining
            - 基于graph-based backend
            - 支持手动调节
    

- innovations(与FloPoCo相比)
    - just-right: more than philosophy
        - 通过传播范围的类型系统实现真正的自动just-right设计
        - 位宽-范围-多项式的信息层次结构和稳健的坍塌机制
    - synthesis & verification: verifying on the fly
        - synthesis
            - 通过tcl writer和主流EDA交互
        - verification
            - 利用SpinalHDL的仿真接口,真正实现一站式仿真
            - 支持直接的数值输入和输出
            - 支持基于范围生成遍历testCases和随机testCases
            - 标准testbench模板,直接利用时序信息
    - design and reuse: hierarchical & no more black box
        - 不需要生成不可读的设计再使用
        - 真正可读可写的codegen(HGL,而非templated-HDL)
        - 超强的参数化
        - 丰富的第三方支持
        - 层次结构哲学 → 自动从下层改善获得收益
    - 下面的内容在第一期之后实现
        - pipelining: auto,right here, right now
            - 通过tag系统支持设置"时序断点"
            - 即使不设置时序断点,也有初步schudling
        - dataflow: DG, without A
            - 支持带有反馈的结构
            - 利用tag系统,在背后由图论模型支持
        - visualization: submit your paper the day after tomorrow
            - 利用tag系统,生成计算流图
                - 运算tag成为相应符号
                - 并且,使用时序断点tag来进行对齐
        - no hesitation: inherit everything from FloPoCo by blackbox

# motivational examples

构造新类型 - ComplexNumber

[使用SpinalHDL实现硬件算法 - Adder Graph](https://www.notion.so/SpinalHDL-Adder-Graph-d45294a6702c4e5d9fd06832d1b29a14)

[使用SpinalHDL设计DSP IP - CORDIC](https://www.notion.so/SpinalHDL-DSP-IP-CORDIC-b70350cda91044dba1cc1b959f1eaab6)

[使用SpinalHDL封装DSP48E2](https://www.notion.so/SpinalHDL-DSP48E2-be9c5eb9cf864737ad037813d2e9878f)

[复数乘法器与DSP](https://www.notion.so/DSP-6d32082c47404f819c680dc9578abcb9)

[qamdemod模块](https://www.notion.so/qamdemod-0b9358579399476fada3b46075e8b636)

[创芯大赛-RSA/ECC](https://www.notion.so/RSA-ECC-01dc7dc6ad774b39a36d13d55d273a52)

# 实战项目

[创芯大赛-NTT](https://www.notion.so/NTT-42b15f2aef8344a9ac129e6b778781cd)

[创芯大赛-SM4-GCM](https://www.notion.so/SM4-GCM-144cc5298f944ad7b3f74f4deb4c685d)

[赛事集训记录](https://www.notion.so/5d98585ccf46485dbfda38fb1a2d1b60)

施工中

[使用SpinalHDL实现硬件算法 - (你以为)人人都懂的FFT](https://www.notion.so/SpinalHDL-FFT-e277a19c34244cb68d5cb7ca90771f95)

[使用SpinalHDL组织代码复用 - WinoGrad for DNN](https://www.notion.so/SpinalHDL-WinoGrad-for-DNN-6aaf125c90c64c26805722cb8b89ce96)

[使用SpinalHDL实现割集重定时](https://www.notion.so/SpinalHDL-2882ab91bb0a4894bf3fd87007bd8f1e)

[使用SpinalHDL实现"架构" - Trees](https://www.notion.so/SpinalHDL-Trees-e7953bb053244d218904e12d448e34ab)

# 同类工作对比

- Chisel-DSP
    - [https://github.com/ucb-bar/dsptools](https://github.com/ucb-bar/dsptools)
    - 提供了框架,但没有提供设计
    - 许多想法和我一致
    - 一段时间没有更新了,我可以做得更好

- FloPoCo
    - 
    - 提出了很多有趣的想法,但(一定程度上,受限于当时的技术)实现手段的自动化不足
    - 有较多的内建设计
    - 可以借鉴其设计理念,一对一cover
    - 可以借鉴其对于设计的分级和分类
    - 可以调用/移植其现有设计

- Spiral
    - DARPA资助,CMU维护的数值计算软/硬件生成器
    - 还没有仔细了解,但他们的研究重点似乎是数值计算本身
    - 虽然有几个令人影响深刻的在线verilog生成器,但总的来说,这个项目并不是面向RTL设计的
    - 与FloPoCo同样地,也许可以使用这个项目作为我的后端
    - 实际上,$H_\mathrm{cub}$算法的发明者是这个项目的领导者之一

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
    
    
- 实现这些特性的关键工作
    - 实现支持range传播的数值类型
    - 提取网表作为后端数学模型,并且融合tag系统
        - 用于支持自动pipeline
        - 用于支持visialization
- 关键开发节点
    - 完成关键数值类型和位宽的自动裁剪,制作demo对标Hardware Division by Small Integer Constants,最好是ppt/视频demo
    - 完成网表提取功能和网表的图论化
    - 完成自动pipeline
    - 完成visualization

# 内容

## IPs

### Coding style

- 设计
    - 代码复用方法
        - 连线算法通过def实现
        - 调用接口通过ImplicitArea实现,这也是要实现DSPDesign的层次
        - 测试用例通过Component实现
    - 参数与设计要求
        - 一个典型Chainsaw设计接收以下输入
            - 输入信号
            - 类型参数,尽量直接通过输入信号提供
            - 设计参数
            - 架构参数,继承自DSPArch
            - pipeline参数
                - 当PipeArch = NONE时,所有计算核都是组合逻辑
                - 当PipeArch = ONE时,只在结尾插入delay
        - 时序信息方法
            - 利用DSPDesign所提供的时序信息,可以自动实现
                - DSPSim时序控制
                - AXI-4 Stream
                - Avalon?

- 文档
    - 书面文档,记录在 [](https://www.notion.so/e3a31d826f1f4d95aae0f22dc1ab5258) 中每个条目的页面上,内容包括
        - 数学公式
        - 架构图
        - 合法输入范围
    - 代码注释,记录在ImplicitArea上方,内容包括
        - 功能摘要,以及到文档页面的链接
        - 参数说明

- 功能测试
    - 应当实现trait DSPSim
    - 使用Breeze作为reference model
    - 功能测试要在三个维度上展开
        - 所有合法的信号
        - 所有合法的设计参数
        - 所有合法的类型参数
        

- 性能测试

### 具体任务

- 总体
    - [ ]  设计整个工程的自动测试
    - [ ]  调整package中的类型策略
        - [ ]  对于有具体含义的值实现相应类型,比如三角函数相位值,三角函数返回值
        - [ ]  解除实现的IP对于data类型的依赖,实现细粒度的类型参数化,增加类型参数,而data应当作为默认值存在
    - [ ]  把Chainsaw迁移到新的project中,对SpinalHDL的依赖改成本地的版本
        
        [新的FixedPoint类型](https://www.notion.so/FixedPoint-b77d4aead5a84e53b689e020968b61bd)
        
        - [ ]  根据实际需要重写FixedPoint,第一步是像之前的Xilinx库一样,采用非侵入的覆盖式方法实现
            - [ ]  深入了解SpinalHDL类型系统
            - [ ]  深入了解SpinalHDL import系统,module,package
            - [ ]  深入了解Git以及GitHub的fork机制
        - [ ]  一种不太优雅,但有用的方式 - 使用implicit
    - [ ]  编写设计/生成/测试的Componnet/Area code template
    - [x]  编写进行SpinalHDL import的live template
    - [x]  编写进行Breeze import的live template
    - [x]  实现package function
    - [ ]  将ComplexNumber改写成Data类型,支持Reg,可以用作端口
    - [ ]  根据"测试平台"结构,编写标准的测试类
        - 具体测试类从标准测试类派生,需要实现标准测试类的方法
        - 从已经实现的FFT测试平台中,提取在整个DSP库中可复用的要素
        - [x]  实现刻画测试平台结构的trait
        - [ ]  实现刻画队列测试和输入/输出/工作/保护/退出距离的trait
    - [ ]  研究Breeze,研究它能为哪些计算核提供reference model
    - [x]  增补SFix/UFix的文档
    - [ ]  将已经写了的代码按照复用性要求重构
    - [ ]  DSPDesign应该要求实现last & valid & 启动方法,这些方法的启用会带来可选的逻辑,就好像各个lib设计当中的when一样
    - [ ]  再仔细考虑outputregistration策略
        - [ ]  一个可能的做法是,一方面总是进行,另一方面自动删余

- 类型设计
    
    [复数类型](https://www.notion.so/35c1db44e271442e91343a8b3590b69a)
    

- Xilinx库和工具链
    - [ ]  在Xilinx库中实现方法提取Detailed RTL Component Info
    - [ ]  部署服务器,用于大规模.多线程综合和仿真,需要查看Xilinx关于多线程综合的资料
    - [ ]  
    

- RAG
    - [ ]  将"加法树"进一步实现为"移位-加法树",再进一步实现为Adder Graph
        - [ ]  阅读文献,了解Adder Graph硬件映射的细节
        - [ ]  从JGraphT中构造一个用于表示Adder Graph的数据类型,作为开展后续工作的基础
        

- FFT
    - [x]  对FFT进行函数化改写
    - [ ]  测试和修正精度范围
        - [ ]  FFT输出范围应当与输入不同
    - [ ]  编写FFT的完整功能测试平台
        - [ ]  formal测试平台
        - [ ]  完整测试集
            - [x]  随机化测试 - 数值/精度测试上,随机测试应该就具有良好的覆盖率
            - [ ]  corner case测试 -
    - [ ]  architecture considerations
        - [ ]  实现FFT的pipeline策略
        - [ ]  考虑FFT和脉动阵列的结合
    - [ ]  编写FFT的性能测试平台
        - [ ]  与IP对照的方法,是否有tcl指令可以传递参数,生成IP
        - [ ]  提取Xilinx Report当中的资源占比数据
        - [ ]  提取综合过程报告中的问题
    - [ ]  功能扩展
        - [ ]  实现对于实序列的2N-N接口
        

- FIR
    - [ ]  功能扩充
        - [ ]  实现MAC架构
            - [x]  systolic的基本实现
            - [ ]  对称系数优化
        - [ ]  实现DA架构
        - [ ]  实现RAG架构
    - [ ]  性能验证
        - [ ]  验证systolic架构在FPGA DSP slice上的精确映射

- CORDIC
    - [ ]  功能扩充
        - [ ]  实现初始变换,扩大收敛域
        - [ ]  实现所有功能模式的API
        - [ ]  增加数据合法性检查电路
        - [ ]  增加处理器模式
    - [ ]  性能验证
        - [ ]  验证设计在特定功能模式下的"常数化坍塌"

# 进度记录和进度管理

[Chainsaw开发路线图](https://www.notion.so/fdfa649cf7d34f41adc53cab5ddab890)

[Chainsaw Mechanism](https://www.notion.so/d66d9d6ef1f542bf9e35e5408351a363)

[Chainsaw Designs](https://www.notion.so/e3a31d826f1f4d95aae0f22dc1ab5258)

## 新的数值类型

## just right design框架

参考文档

[AMBA 4 AXI4-Stream Protocol及其SpinalHDL实现](https://www.notion.so/AMBA-4-AXI4-Stream-Protocol-SpinalHDL-bf121ed9f071478388dc42e7ee2c8b2b)

chainsaw生成后的设计仍然是对象,而不仅是文本

制作一组位宽输出策略帮助优化设计

[](https://www.notion.so/03d36c8ebdc7455994a1b1e428758bda)

[Page title...](https://www.notion.so/Page-title-d2aa3f52a0ed4022b7820473fa9e5bc2)

[Page title...](https://www.notion.so/Page-title-6b034c69201449088459342914616c36)

[fig2](https://www.notion.so/fig2-f7409c0b9a5245a2b9ccccc5fde182ce)

[sn](https://www.notion.so/sn-a49690b9a097455daab1d6c49dd70e5d)

[proof](https://www.notion.so/proof-dad1ccfcad4648eab28ba6db8391ff6a)

[snfix](https://www.notion.so/snfix-05085fad82b544d9b8fbb0d4ae3039a9)

[不输出的内容](https://www.notion.so/f3b0ce2bbb7948ffb5451479444fc96d)

[Page title...](https://www.notion.so/Page-title-0e066e4a35b64e479369439b76fdd7c8)

[Page title...](https://www.notion.so/Page-title-f9146c181f744983890eaef303d5c88b)

[figs](https://www.notion.so/figs-2d73b72deae74cd096dc4c414b38a5a5)

[Readme 0.9](https://www.notion.so/Readme-0-9-da6e10c716544be8b7eed30c8e832b48)
