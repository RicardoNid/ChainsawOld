import ai.djl.*;
import ai.djl.nn.*;
import ai.djl.nn.core.*;
import ai.djl.training.*;

import ai.djl.nn.Activation.*;
import ai.djl.nn.Blocks.*;
import ai.djl.nn.core.Linear.*;

import java.nio.file.*;

import ai.djl.*;
import ai.djl.basicdataset.cv.classification.Mnist;
import ai.djl.ndarray.types.*;
import ai.djl.training.*;
import ai.djl.training.dataset.*;
import ai.djl.training.initializer.*;
import ai.djl.training.loss.*;
import ai.djl.training.listener.*;
import ai.djl.training.evaluator.*;
import ai.djl.training.optimizer.*;
import ai.djl.training.util.*;
import ai.djl.basicmodelzoo.cv.classification.*;
import ai.djl.basicmodelzoo.basic.*;

public class App {
    public static void main(String[] args) {
        Application application = Application.CV.IMAGE_CLASSIFICATION;
        long inputSize = 28 * 28;
        long outputSize = 10;
        SequentialBlock block = new SequentialBlock();
        block.add(Blocks.batchFlattenBlock(inputSize));
        block.add(Linear.builder().setUnits(128).build());
        block.add(Activation::relu);
        block.add(Linear.builder().setUnits(64).build());
        block.add(Activation::relu);
        block.add(Linear.builder().setUnits(outputSize).build());

        System.out.println(block);
    }
}
