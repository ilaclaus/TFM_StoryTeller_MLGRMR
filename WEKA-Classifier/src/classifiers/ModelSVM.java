package classifiers;
import weka.classifiers.functions.LibSVM;
import weka.core.SelectedTag;

public class ModelSVM extends Model {
	public ModelSVM() {
		super(new LibSVM());
	}
	
	public ModelSVM(int kernelType) {
		super(new LibSVM());
		
		((LibSVM) super.cls).setKernelType(new SelectedTag(kernelType, LibSVM.TAGS_KERNELTYPE));
	}
}
