/*
 * Iván Manuel Laclaustra Yebes
 * DNI: 50909141-K
 * Mail: ilaclaus@ucm.es
 * 
 * Trabajo de fin de máster en ingeniería informática de la UCM
 * Generación de Narrativa basada en Criterios de Calidad Aprendidos Automáticamente
 */

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
