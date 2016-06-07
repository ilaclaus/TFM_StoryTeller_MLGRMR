/*
 * Iván Manuel Laclaustra Yebes
 * DNI: 50909141-K
 * Mail: ilaclaus@ucm.es
 * 
 * Trabajo de fin de máster en ingeniería informática de la UCM
 * Generación de Narrativa basada en Criterios de Calidad Aprendidos Automáticamente
 */

package classifiers;
import weka.classifiers.trees.RandomForest;

public class ModelRF extends Model {
	public ModelRF() {
		super(new RandomForest());
	}

}
