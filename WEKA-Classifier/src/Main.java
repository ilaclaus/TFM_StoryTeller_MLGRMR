import classifiers.Model;
import weka.classifiers.functions.LibSVM;
import classifiers.*;

public class Main {

	public static void main(String[] args) {
		// Probar con diferentes kernels
		Model model = new ModelSVM(LibSVM.KERNELTYPE_POLYNOMIAL);
		
		model.trainAndTest();
		model.predict();
	}

}
