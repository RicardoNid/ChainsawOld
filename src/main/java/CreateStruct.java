
import com.mathworks.engine.*;
import com.mathworks.matlab.types.*;

public class CreateStruct {
    public static void doit() throws Exception {
        MatlabEngine eng = MatlabEngine.startMatlab();
        double[] y = {1.0, 2.0, 3.0, 4.0, 5.0};
        HandleObject h = eng.feval("plot", y);
        eng.eval("pause(5)");
        double[] color = {1.0, 0.5, 0.7};
        Struct s = new Struct("Color", color, "LineWidth", 2);
        eng.feval("set", h, s);
        eng.eval("print('myPlot', '-djpeg')");
        eng.close();
    }
}
