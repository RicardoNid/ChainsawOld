# 技术和理论基础

本章介绍了Chainsaw所依赖的基本技术和理论.在编程架构上,Chainsaw的实现是基于SpinalHDL语言和Scala语言的.在理论模型方面,本文沿用了[1]中对于线性变换的定义和对于硬件重用的形式化.因此,在本章的后续各节中,2.1节介绍了SpinalHD框架和Scala语言的特性,以及Chainsaw与它们的协同方式.2.2节概括了[1]所提出的重要贡献和局限性,对比了Chainsaw与[1]的异同.2.3节说明了[1]中对线性变换的递归定义.2.4节介绍了[1]中的两种对硬件进行重用的方式.

# Scala语言和SpinalHDL语言

## Scala语言

Scala是洛桑联邦理工学院的Martin Odersky于2004年发布的一种多范式的高级编程语言,Scala首先在大数据和并行编程领域获得成功,之后又被广泛地应用于DSL开发.本文提出的框架所基于的硬件编程语言,SpinalHDL,与更具知名度的另一门硬件生成语言Chisel[1]一样,是嵌入在Scala语言中的.实际上,还有一些与硬件设计相关的DSL,如为机器学习加速器而设计的Spatial[2],也是嵌入在Scala中.而在硬件描述语言这一领域之外,还有大量其它领域的DSL选择Scala作为宿主语言.一定程度上,Scala统治了DSL开发这一领域.

Scala被大量的DSL选为宿主语言,其原因在于以下的特性:

1. Scala是静态类型,强类型语言.因此,嵌入的DSL能够利用其编译阶段的静态类型检查来保证DSL中的类型安全.
2. Scala是一种运行在Java虚拟机(JVM)上的语言,并大量地支持与Java的互操作.这意味着首先,Scala程序和JVM一样,是天然跨平台的,只要目标系统上有JVM的发行版本,Scala程序就能运行.其次,通过与Java的互操作,Scala不仅能够从Scala社区,还能够从Java社区获取大量的第三方库和工具支持,而Java拥有高级编程语言中最庞大的生态之一.一个例子是,Chainsaw在部署时,所有的库依赖都可以通过Maven中央仓库获取.
3. Scala是一种多范式的编程语言,其中最重要的两个范式是1)面向对象编程,2)函数式编程.通过面向对象编程特性,如继承,混入,多态等,Scala提供了良好的多层次抽象和代码复用能力.通过函数式编程特性,如一等函数,高阶函数,不可变对象和集合等,Scala提供了实现纯函数,函数复合,以及操作函数对象的直接方法.
4. Scala本身为支持DSL实现了大量的特性,其中最重要的是1)类型推断,2)中缀表达式和3)隐式.类型推断省去了大量不必要的类型注释,使DSL的表达更加自然,而不是总是被类型信息打断.中缀表达式允许设计者通过类似于数学公式的方法,对对象的方法进行调用,这对实现类公式(formula-like)的DSL很有意义.隐式将”背景信息”隐藏到隐式类,隐式方法或隐式对象,在实现DSL时,隐式可以用于实现一门DSL的”语境”.一个例子是,在Chainsaw中,通过访问当前编程环境中以隐式对象方式提供的向量空间,编译器可以判断当前的运算是发生在有限域还是整数环,尽管两者的表面形式是相同的.

## SpinalHDL语言

SpinalHDL是2016发布的一种,嵌入于Scala中的硬件生成语言.相比传统HDL(verilog/VHDL),SpinalHDL首先在底层提供了与verilog/VHDL同等的,细颗粒度的描述能力,但是提供了更加清晰的,对于基本硬件要素,如寄存器和存储的抽象,这使得SpinalHDL至少具有与传统HDL同等的描述能力.在此基础上,SpinalHDL利用Scala的特性,提供了verilog/VHDL所无法实现的特性,包括复杂类型系统,高度抽象能力,编译期的静态检查,生成期的语义检查等.

### SpinalHDL的硬件建模

SpinalHDL提供了完整的,与传统HDL对应的语法要素,但是在时序逻辑方面,包括寄存器,RAM/ROM,以及时钟域的描述上,提供了更加清晰的建模.同时,SpinalHDL也摒弃了暧昧不清的”赋值”概念,采用了有直观硬件对应的”连接”概念.下表中给出了其与verilog的语法要素对应关系.

| SpinalHDL 语法要素 | (System)Verilog 语法要素 | 分类 |
| --- | --- | --- |
| in/out | input/output | I/O |
| Bundle | interface | I/O |
| ClockDomain | 敏感信号列表和always块 | 时钟域 |
| signals(Bool,Bits,UInt,Sint and Vec) | wire/reg/logic and its modifiers | 信号和连接 |
| 连接(:=) | 赋值(=, <=) | 信号和连接 |
| when/elseWhen/others | if/else if/else | 组合逻辑 |
| switch/is/default | case/default | 组合逻辑 |
| logical operators | logical operators | 组合逻辑 |
| relational operators | relational operators | 组合逻辑 |
| arithmetic operators | arithmetic operators | 组合逻辑 |
| registers | 用其它要素描述其行为,交由综合器推断 | 时序逻辑 |
| RAM/ROM | 用其它要素描述其行为,交由综合器推断 | 时序逻辑 |

SpinalHDL与Verilog的语法要素对照

### SpinalHDL的抽象能力

在完备且更加清晰的硬件建模的基础上,SpinalHDL利用Scala提供的特性,提供了高度的抽象能力,包括但不限于:

1. 通过类型系统和面向对象特性,设计者可以在接口设计方面上实现复杂的,带有约束的接口类型;在模块设计方面,以继承方式实现模块间的代码复用,以特征(trait)方式给出模块设计所需要遵守的规则.
2. Scala标准库提供了强大的集合类型(类似于C++)容器和集合方法.设计者能够以一种类似于软件工程中的泛型的方式,通过标准化的集合方法和对高阶函数和递归的支持,对与具体实现无关的硬件结构进行简洁的描述.一个例子是代码段2-1中,高阶函数butterfly接收描述$F_2$模块的一阶函数f2,通过递归调用自身和使用标准集合方法(map,grouped)等,紧凑地描述了一个以$F_2$模块为基础的硬件蝶形网络.
- [x]  图2-1 蝶形网络
- [x]  代码2-1 蝶形网络代码
1. 通过诸如ScalaTest的测试框架,设计者可以高效地组织和管理测试,并且在不仅在设计,也在测试方面进行代码复用.

### SpinalHDL的工具链

1. 在编译和运行环境上,SpinalHDL只需要标准的Scala编译器即可编译,可以在任何有JVM发布的机器上运行,而无需其它依赖.
2. 在部署上,SpinalHDL利用了Scala成熟的包管理系统和build工具,通过SpinalHDL完成的硬件生成器能够像其它高级语言的开源软件包一样,通过包管理系统打包,通过统一的中央仓库(Maven)分发到使用者开发环境,然后通过统一的build工具(sbt或mill)部署.

### SpinalHDL与Chisel

Chisel[1]是加州大学伯克利分校所开发的硬件生成语言,其开发契机和典型应用是RISC-V处理器的设计.在对于硬件的基本建模上,SpinalHDL与Chisel非常相似.不过,Chisel在生成传统HDL代码之前,会先被转换为名为FIRRTL[4]的中间表达形式,因此有相对更加复杂的编译流程,这影响了Chisel生成的HDL代码的可读性,为一些验证工程师所诟病.选择SpinalHDL作为实现基础是出于以下三个原因: 1)FIRRTL中间语言是为实现与后端无关的优化和等价变换而提出的,但本文对设计的优化在框架内直接完成,因此没必要经过FIRRTL. 2)SpinalHDL生成的HDL代码可读性更好. 3)作者更熟悉SpinalHDL的技术栈,并获得了其社区的支持.

## 本框架与Scala,SpinalHDL的关系

本质上,本文的工作是”生成器语言无关”的,因此,无论是Chisel还是SpinalHDL,都可以作为Chainsaw的后端使用.从编译器的观点出发,SpinalHDL/Chisel所负责的工作是代码生成(图2-3).在包含关系上,Chainsaw和SpinalHDL都是以Scala语言编写的库,互相没有包含关系,都从属于Scala语言.在依赖关系上,Chainsaw的实现依赖于SpinalHDL和Scala(图2-4).

- [x]  图2-2 编译器视角下的Chainsaw,SpinalHDL和Chisel
- [x]  图2-3 Scala,SpinalHDL和Chainsaw的包含关系

# 基于公式的数值计算描述模型

在本文提出的框架之前,[1]对于以数学形式对硬件进行形式化描述就进行了一些尝试.[1]实现了一个以公式形式描述执行线性变换的硬件,并对其进行编译的系统.

### 重要贡献

[1]中提出了以下的重要观点:

1. [1]中对其框架可实现的线性变换,通过巴科斯范式(Backus-Naur Form, BNF)给出了明确的递归定义,这一递归定义揭示了如何将线性变换分解为有明确硬件设计语义的基本线性变换,指引了后续的编程实现方案.这在2.3节进行回顾和扩展.
2. [1]中对于两种重要的硬件重用范式: 流重用和迭代重用给出了形式化的表达,这指引了其编译流程中的硬件重用的实现.这在2.4节中进行回顾和扩展.

### 局限性

[1]所描述的数值计算系统有以下的约束:

1. 不支持跨越类型的变换(因此两个类型参数都是`T`).
2. 所有变换都是线性变换.
3. 所有线性变换都对应于方阵,因此输入和输出向量尺寸相同.

如果类比本文在第一章中给出的系统定义`ref`,[1]中所描述的数值计算系统可以被记为

$$
\mathrm {LinearTransform: Vector_n[T] => Vector_n[T]}
$$

对照Chainsaw所描述的系统(公式1-1),显然,[1]所描述的系统是本框架所描述系统的子集,这一变换的输入/输出类型和尺寸更加受限,并且内容局限于线性变换.上述约束的存在,使得[1]的可用性被严重限制,因为在实际应用场景中,数值计算的输入/输出向量常常具有不同的类型,不同的尺寸,常常是非线性的.一个例子是QAM映射,其输入为无符号整数,输出为复数,是一个跨越不同类型的非线性变换.实际上,[1]中所讨论的变换和进行的实验仅限于DFT和DCT的实现.[1]中讨论了其框架在设计光学OFDM系统时的应用,但也仅局限于系统中的FFT/IFFT部分.

## Chainsaw与[1]的异同

在Chainsaw中,对于变换的范围,硬件重用的方法,系统吞吐率的匹配和编译流程的实现都做了大量的扩充和修改.本文将线性变换扩展到一般变换,将两种固定的重用方法扩展到为三种,增加了滑窗重用方法,并提供了对于一般变换的重用的设计接口,使设计者可以自行扩展可重用的硬件.本文将紧耦合的黑箱编译流程,更改为模块化的,可由设计者扩展的编译流程.不过,本文的框架在线性变换和硬件重用上沿用了[1]中的大部分定义.因此,在介绍本框架的内容之前,有必要简要回顾[1]中的核心内容,即对于线性变换的定义,和对于硬件重用的定义.

|  | [1] | Chainsaw |
| --- | --- | --- |
| 描述范围 | 输入输出尺寸,类型一致的线性变换 | 输入输出具有任意尺寸,类型的一般变换 |
| 硬件重用方法 | 两种固定方法
每种基本线性变换有固定的对于重用方法的实现 | 增加了滑窗重用方法
设计者对自定义的一般变换,可以依据设计接口实现重用 |
| 吞吐率匹配 | 只支持几种系统具有相关知识的常见变换,设计者通过directive间接指定吞吐率 | 支持所有能够被Chainsaw公式表达的系统,设计者直接指定定量的相对吞吐率 |
| 编译流程 | 依照传统编译器方式设计,代码分析和生成过程紧耦合,难以扩展. | 以SpinalHDL作为代码生成器,代码分析和生成过程分离,流程高度模块化,设计者可以通过重载模块接入流程. |

# 线性变换

本节回顾[1]中对于线性变换的定义.

在对于线性变换的讨论中,本文采用下面的符号:

- 使用大写字母代表矩阵,即线性变换,带有向量符号的小写字母来代表向量,带有两个下标的小写字母来代表矩阵中的元素,带有一个下标的小写字母来代表向量中的元素.

$$
A  =\left[\begin{array}{cccc}
a_{0,0} & a_{0,1} & \cdots & a_{0, n-1} \\
a_{1,0} & a_{1,1} & \cdots & a_{1,n-1} \\
\cdots & \cdots & \cdots & \cdots \\
a_{m-1,0} & a_{m-1,1} & \cdots & a_{m-1, n-1}
\end{array}\right]
$$

$$
\vec v = (v_0, v_1,\cdots v_n)
$$

- 没有特别说明时,本文用$\vec x$代表尺寸为$n$的输入向量,用$\vec y$代表尺寸为$m$的输出向量.线性变换定义在
- 没有特别说明时,记线性变换发生的向量空间为$\mathbb V$,$\mathbb V$定义在域$\mathbb F$上,相应的概念,如单位元,零元等,都对应于$\mathbb F$
- 没有特别说明时,一个向量是列向量,矩阵-向量乘法中,矩阵在前,向量在后.

## 矩阵分解

一个n-to-n线性变换通过一个$n \times n$矩阵定义,对输入向量进行线性变换的过程等价于一个矩阵-向量乘法的过程.一个例子是$n$点向量上的离散傅立叶变换,可以被写为$\vec y = \mathrm {DFT}_n\vec x$,其中

$$
⁍
$$

展开的矩阵形式为

$$
⁍
$$

这种是定义完备的,但对于硬件实现的帮助不大,因为在实际的硬件实现中,不可能对任意尺寸的矩阵乘法以完全展开的方式进行实现,这种方式没有揭示出许多矩阵内在的,有规律的重复.[1]中通过编译器领域常用的巴科斯-诺尔范式(BNF)给出了对于线性变换的递归定义,以表达从几种基本线性变换出发,通过运算构造任意线性变换的过程.

$$
\operatorname{matrix}::=\operatorname{matrix} \cdots \operatorname{matrix} | \prod_l \operatorname{matrix} | I_k \otimes \operatorname{matrix} | \operatorname{base}
$$

$$
\operatorname{base}::=D|P|A
$$

为了不熟悉这一范式的读者,本文对其进行进一步的解释,在BNF表达式中,$::=$读作”定义为”,$|$读作”或”,通过枚举一个语素的所有可能情况,上面的范式给出了一个语素的定义.如果一个语素的某一定义中包含了它自己,那么这一定义是递归的,所有定义中一定有一个不包含该语素的定义,作为递归的基本情形.

定义式a给出了通过运算从基本矩阵产生矩阵的方法,[1]中的一个矩阵是以下情形之一:

1. 多个不同矩阵的积 $\operatorname{matrix} \cdots \operatorname{matrix}$
2. 一个矩阵的幂 $\prod_l \operatorname{matrix}$
3. 单位阵与一个矩阵的克罗内克积 $I_k \otimes \operatorname{matrix}$
4. 是一个基本矩阵 $\operatorname {base}$

前三种情形都是递归的,第四种情形是递归基础,因此,任意矩阵实际上都是从基本矩阵出发,通过前三种情形中的运算而得到的.

定义式b定义了基本矩阵,一个基本矩阵是以下情形之一

1. 排列矩阵$P$
2. 对角矩阵$D$
3. 代数矩阵$A$

这一定义是枚举的,从上面的定义出发,只要实现三种基本矩阵,并实现两种运算方式,积(积也定义了幂)和克罗内克积,就能实现对任意局真的构造

## 基本矩阵

### 排列矩阵

排列矩阵$P$,排列(permutation)是一种特殊的线性变换,排列的输出与输入具有相同的元素,但具有不同的顺序.一个排列矩阵指定了一种固定的排列,矩阵的每一行,每一列只有一个元素是单位元,其它元素是零元.

将排列前后的元素顺序通过下标进行表达,可以记为

$$
P(\vec x) = P((x_0, x_1 \cdots x_{n-1})) = (x_{o_0}, x_{o_1} \cdots x_{o_{n-1}})
$$

显然,排列$P$可以被顺序向量$\vec o = (o_0, o_1 \cdots o_{n-1} )$定义,对于变换矩阵$P$中的元素$p_{i,j}$,有

$$
p_{i,j} = \left\{\begin{array}{l} 1 ,\quad j = o_i\\0, \quad \mathrm {otherwise}\end{array}\right.
$$

下面给出一个4*4排列矩阵的例子,对于$\vec o = (1,0,3,2)$,应当有

$$
P = \left[\begin{array}{cccc} 0 & 1 & 0 & 0 \\ 1 & 0 & 0 & 0 \\ 0 & 0 & 0 & 1 \\ 0 & 0 & 1 & 0 \end{array}\right] \quad P \cdot\left[\begin{array}{cccc} 0 \\ 1 \\ 2 \\ 3 \end{array}\right] = \vec o
$$

特别地,在许多实际工程问题中,有一项重要的排列,stride-by-k permutation,在通信中这一排列被称为块交织,这一交织使得原本相距$k$的数据在排列后相邻,一种直观的理解方式是,对于长度为$jk$的向量,将其以row-major方式输入$j\times k$矩阵,然后以column-major方式输出,就完成了这一排列.我们将其记为$\mathrm{SP}_{j,k}$

下面给出一个$\mathrm {SP}_{2,4}$的排列过程作为例子

$$
\left[\begin{array}{cccccccc} 0 \\ 1 \\ 2 \\ 3 \\ 4 \\ 5 \\ 6 \\ 7 \end{array}\right]
\rightarrow \left[\begin{array}{cc} 0 & 1 & 2 & 3 \\ 4 & 5 & 6 & 7 \end{array}\right]
\rightarrow \left[\begin{array}{cccccccc} 0 \\ 4 \\ 1 \\ 5 \\ 2 \\ 6 \\ 3 \\ 7 \end{array}\right]
$$

### 对角矩阵

对角矩阵也代表了一种特殊的线性变换,其特殊性在于在变换过程中,输入的各个元素之间不会发生关系,输出的每个元素独立地依赖于一项输入元素.这在许多场景中有直接的应用,如载波通信系统中,对于每个子载波进行独立的信道均衡.

对角矩阵可以通过其对角线上的元素定义,记为$\vec d = (d_0, d_1 \cdots d_{n-1} )$,展开的矩阵形式为

$$
D =\operatorname{diag}\left(d_{0}, d_{1}, \ldots, d_{n-1}\right)=\left[\begin{array}{llll}d_{0} & & & \\& d_{1} & & \\&  & \dots & \\& & \\& & & d_{n-1}\end{array}\right]
$$

变换后的向量为

$$
D(\vec x) = (d_0x_0, d_1x_1 \cdots d_{n-1}x_{n-1})
$$

下面给出一个4*4对角矩阵的例子,对于$\vec d = (1,2,3,4)$,应当有

$$
\left[\begin{array}{cccc} 1 & 0 & 0 & 0 \\ 0 & 2 & 0 & 0 \\ 0 & 0 & 3 & 0 \\ 0 & 0 & 0 & 4 \end{array}\right]
$$

### 代数矩阵

代数矩阵是一个一般的矩阵,其变换过程就是一个矩阵-向量乘法,即$\vec y = A \vec x$,而没有更加简单的实现方式.实际上,在大部分实际算法中,代数矩阵的尺寸较小,并且是稠密的.因为一个复杂的变换往往是由多个更小的,重复的变换通过运算构出来的.一个例子是两点的离散傅立叶变换矩阵.

$$
⁍
$$

## 矩阵运算

通过矩阵运算,简单的矩阵可以被用于构造复杂的矩阵,[1]中所支持的矩阵运算有两类

### 矩阵乘法

一个矩阵可以被分解为矩阵的乘积.同一矩阵的迭代乘积(幂)很重要,因为这一表达形式显式地指出了同一变换的重复使用,这指引了后续的硬件重用.

通过矩阵乘法得到的新变换有明确的语义: 对一个向量进行$AB$变换,记为$\vec y=(AB)\vec x$,等价与在$\vec x$上先进行变换$B$,然后进行变换变换$A$,应当注意,这一顺序是从右往左的.

![2-4 矩阵乘法](%E6%8A%80%E6%9C%AF%E5%92%8C%E7%90%86%E8%AE%BA%E5%9F%BA%E7%A1%80%20fc40d46356ab4bc1ae0ce73f5bd9a219/Untitled.png)

2-4 矩阵乘法

### 与单位阵的张量积(克罗内克积)

张量积(或克罗内克积)的定义如下

$$
⁍
$$

展开的矩阵形式如下

$$
\left[\begin{array}{ccc}b_{0,0} \mathbf{A} & \cdots & b_{1, n-1} \mathbf{A} \\\vdots & \ddots & \vdots \\b_{m-1, 0} \mathbf{A} & \cdots & b_{m-1, n-1} \mathbf{A}\end{array}\right]
$$

[1]中仅允许克罗内克积的第一个操作数为$k \times k$单位矩阵$I_k$,展开形式为

$$
I_{k} \otimes A=\left[\begin{array}{llll}A & & & \\& A & & \\& & \ddots & \\& & & A\end{array}\right]
$$

展开后的矩阵尺寸为$km \times kn$,这样的矩阵被称为块对角(block-diagonal)矩阵.

[1]在其框架中支持这一运算的原因是,这一变换有明确的语义: 对一个向量进行$I_{k} \otimes A$变换,等价与对其$k$等分后的$k$个子向量并行地进行$A$变换.

![2-5 与单位阵的克罗内克积](%E6%8A%80%E6%9C%AF%E5%92%8C%E7%90%86%E8%AE%BA%E5%9F%BA%E7%A1%80%20fc40d46356ab4bc1ae0ce73f5bd9a219/Untitled%201.png)

2-5 与单位阵的克罗内克积

# 硬件重用

通过2.3中描述的表示法,可以表示大量线性变换.但如果在硬件上完全展开这些变换,往往是过于庞大的.为此,需要对硬件计算单元进行重用.[1]中将其称为sequential reuse,来强调这一重用是在不同的时间上反复使用同一计算单元的事实.[1]中支持了硬件重用的两种形式,下面做简单回顾.

## 流重用

流重用(Streaming reuse)将具有并行计算块的系统重构为一个更小的系统,使原始数据分成多段经过多个周期来”流”入系统.流重用与2.3中的张量积是对称的,张量积在垂直方向上引入重复,流重用则对这种重复进行重用.

![2-6 流重用](%E6%8A%80%E6%9C%AF%E5%92%8C%E7%90%86%E8%AE%BA%E5%9F%BA%E7%A1%80%20fc40d46356ab4bc1ae0ce73f5bd9a219/Untitled%202.png)

2-6 流重用

## 迭代重用

迭代重用将具有级联(cascaded)计算块的系统重构为一个更小的系统,是原始数据流通过反馈通路在计算单元上迭代.迭代重用与2.3中的矩阵的幂是对称的,矩阵的幂在水平方向上引入重复,迭代重用则对这种重复进行重用.

![2-7 迭代重用](%E6%8A%80%E6%9C%AF%E5%92%8C%E7%90%86%E8%AE%BA%E5%9F%BA%E7%A1%80%20fc40d46356ab4bc1ae0ce73f5bd9a219/Untitled%203.png)

2-7 迭代重用

在第三章中,将线性变换扩展为一般变换之后,本文将给出上面两种硬件重用的符号定义和更多讨论,并引入更多重用方式.

# 本章小结

本章介绍了本文所提出的框架所依赖的基本技术.首先介绍了作为宿主语言的Scala的特性,然后介绍了作为代码生成器的SpinalHDL的特性,指出了本框架与这两者的关系.然后,对于重要的前置工作[1],本章回顾了其核心观点,展开阐述了其对于线性变换和硬件重用的定义,指出了其工作的不足之处和本文的改进方向.