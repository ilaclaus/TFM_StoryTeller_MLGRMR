/*
 * Iván Manuel Laclaustra Yebes
 * DNI: 50909141-K
 * Mail: ilaclaus@ucm.es
 * 
 * Trabajo de fin de máster en ingeniería informática de la UCM
 * Generación de Narrativa basada en Criterios de Calidad Aprendidos Automáticamente
 */

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
