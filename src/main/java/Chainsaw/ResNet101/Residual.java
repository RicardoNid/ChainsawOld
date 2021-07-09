package Chainsaw.ResNet101;

import java.util.Arrays;

import ai.djl.ndarray.types.*;
import ai.djl.ndarray.*;
import ai.djl.nn.*;
import ai.djl.nn.convolutional.Conv2d;
import ai.djl.nn.norm.BatchNorm;
import ai.djl.training.ParameterStore;
import ai.djl.training.initializer.XavierInitializer;
import ai.djl.util.PairList;

class Residual extends AbstractBlock {
    private static final byte VERSION = 2;

    public ParallelBlock block;

    public Residual(int numChannels, boolean use1x1Conv, Shape strideShape) {
        super(VERSION);

        SequentialBlock b1;
        SequentialBlock conv1x1;

        b1 = new SequentialBlock();

        b1.add(Conv2d.builder() // conv0
                .setFilters(numChannels)
                .setKernelShape(new Shape(3, 3))
                .optPadding(new Shape(1, 1))
                .optStride(strideShape)
                .build())
//                .add(BatchNorm.builder().build()) // BN
                .add(Activation::relu) // relu
                .add(Conv2d.builder() // conv1
                        .setFilters(numChannels)
                        .setKernelShape(new Shape(3, 3))
                        .optPadding(new Shape(1, 1))
                        .build());
//                .add(BatchNorm.builder().build()); // BN

        if (use1x1Conv) {
            conv1x1 = new SequentialBlock();
            conv1x1.add(Conv2d.builder()
                    .setFilters(numChannels)
                    .setKernelShape(new Shape(1, 1))
                    .optStride(strideShape)
                    .build());
        } else {
            conv1x1 = new SequentialBlock();
            conv1x1.add(Blocks.identityBlock());
        }

        block = addChildBlock("residualBlock", new ParallelBlock(
                list -> {
                    NDList unit = list.get(0);
                    NDList parallel = list.get(1);
                    return new NDList(
                            unit.singletonOrThrow()
                                    .add(parallel.singletonOrThrow())
                                    .getNDArrayInternal()
                                    .relu());
                },
                Arrays.asList(b1, conv1x1)));
    }

    @Override
    public String toString() {
        return "Chainsaw.ResNet101.Residual()";
    }

    @Override
    protected NDList forwardInternal(
            ParameterStore parameterStore,
            NDList inputs,
            boolean training,
            PairList<String, Object> params) {
        return block.forward(parameterStore, inputs, training);
    }

    @Override
    public Shape[] getOutputShapes(Shape[] inputs) {
        Shape[] current = inputs;
        for (Block block : block.getChildren().values()) {
            current = block.getOutputShapes(current);
        }
        return current;
    }

    @Override
    protected void initializeChildBlocks(NDManager manager, DataType dataType, Shape... inputShapes) {
        block.initialize(manager, dataType, inputShapes);
    }

    public static NDList getExample() {
        NDManager manager = NDManager.newBaseManager();

        SequentialBlock blk = new SequentialBlock();
        blk.add(new Residual(3, false, new Shape(1, 1)));

        NDArray X = manager.randomUniform(0f, 1.0f, new Shape(4, 3, 6, 6));
        System.out.println(X.getDataType());

        ParameterStore parameterStore = new ParameterStore(manager, true);

        blk.initialize(manager, DataType.FLOAT32, X.getShape());

        Shape shape = blk.forward(parameterStore, new NDList(X), false).singletonOrThrow().getShape();
        System.out.println(shape);

        // another using conv1x1
        blk = new SequentialBlock();
        blk.add(new Residual(6, true, new Shape(2, 2)));

        blk.initialize(manager, DataType.FLOAT32, X.getShape());

        shape = blk.forward(parameterStore, new NDList(X), false).singletonOrThrow().getShape();
        System.out.println(shape);

        return blk.forward(parameterStore, new NDList(X), false);
    }
}