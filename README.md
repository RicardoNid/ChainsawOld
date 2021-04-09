# Chainsaw - an open source, just-right arithmetic core generator(and development fram) based on SpinalHDL

# ideas & innovations

- ideas
    - 强化SpinalHDL定点数 & 浮点数类型,可以参考VHDL库设计
    - 参考BLAS设计,为DSP core分阶数
    - 一个motivational example可以是adder graph的实现,对比FloPoCo CMult中(大概比较丑陋)的代码和Chainsaw中简洁,高可复用性的代码
    - 构建一套数值分析系统,对于每一个operator,实现相应的range演算,在一个component的范围内,每个信号的精度需求取决于独立源的精度信息
        - 需要对于数值类型实现range属性
        - 需要对于数值operator增加tag
        - 需要一个方法提取计算图 - 参考latencyAnalysis的源代码应该能找到方法
            - 计算图中的环?
            - 没有完全展开的计算图?(比如,累加)
- innovations
    - 位宽推断和检查系统
        - 不光进行实现,还要配上理论上的指导
    - 功能导向,而不是硬件导向的API
        - sin(CORDIC),而不仅是CORDIC.sin
    - 基于SpinalHDL,将arithmetic design和以下内容无缝对接
        - 系统级设计
        - 接口设计
        - 测试框架
    - 自动pipeline之外,自动buffer line的实现
- motivational example
    - adder graph for just-right design
    - CORDIC for API design and code reuse

# 同类工作对比

- FloPoCo
    - 特点
        - 非常仔细和严谨的精度分析
    - 区分点
        - 与他们相比,我想制作工程师,而非计算电路研究者的工具
        - 我想实现尽量"无黑箱"的设计
    - 可能的...
        - 直接使用FloPoCo作为SpinalHDL DSP库的后端,SpinalHDL提供黑箱和封装
        - 移植 vs 调用?

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

- Spiral
    - DARPA资助,CMU维护的数值计算软/硬件生成器
    - 还没有仔细了解,但他们的研究重点似乎是数值计算本身
    - 虽然有几个令人影响深刻的在线verilog生成器,但总的来说,这个项目并不是面向RTL设计的
    - 与FloPoCo同样地,也许可以使用这个项目作为我的后端
    - 实际上,$H_\mathrm{cub}$算法的发明者是这个项目的领导者之一

# 设计考量

## numeric consideration

- 精度策略 = 在对方给出数值范围,我方进行数值计算分析前,采用16位定点数,8位小数,数制2C
- 量化策略 =
- 在本项目中,对于定点数的仿真,通过预先将需要的数值移位($\times 2^N$)取整实现

## Coding style

- 设计
    - 应当实现trait DSPDesign
    - 代码复用方法
        - 算法通过def实现
        - 调用接口通过ImplicitArea实现,这也是要实现DSPDesign的层次
        - 测试用例通过Component实现
    - 参数与设计要求
        - 一个典型Chainsaw设计接收以下输入
        - 输入信号
        - 类型参数,尽量直接通过信号提供
        - 设计参数
        - 架构参数,继承自DSPArch
        - pipeline参数
            - 当PipeArch = NONE时,所有计算核都是组合逻辑
            - 当PipeArch = ONE时,只在结尾插入delay
    - 精度要求
        - 对于每一处resized和truncated,都要在注释说明理由

- 文档
    - 书面文档,记录在 [](https://www.notion.so/d5fc835d60f84a0994adb831a5f28f77) 中每个条目的页面上,内容包括
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
- 一些具体实现的coding style记录在 [SpinalHDL CookBook](https://www.notion.so/SpinalHDL-CookBook-947c13f1d6a247c397f63d8f6439fe51) 页面

# 具体任务

- 总体
    - [ ]  调整package中的类型策略
        - [ ]  对于有具体含义的值实现相应类型,比如三角函数相位值,三角函数返回值
        - [ ]  解除实现的IP对于data类型的依赖,实现细粒度的类型参数化,增加类型参数,而data应当作为默认值存在
    - [ ]  把Chainsaw迁移到新的project中,对SpinalHDL的依赖改成本地的版本
        - [ ]  根据实际需要重写FixedPoint,像之前的Xilinx库一样,采用非侵入的覆盖式方法实现
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

[Chainsaw Designs](https://www.notion.so/d5fc835d60f84a0994adb831a5f28f77)
