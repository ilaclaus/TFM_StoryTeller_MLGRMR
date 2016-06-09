/*
 * Iván Manuel Laclaustra Yebes
 * DNI: 50909141-K
 * Mail: ilaclaus@ucm.es
 * 
 * Trabajo de fin de máster en ingeniería informática de la UCM
 * Generación de Narrativa basada en Criterios de Calidad Aprendidos Automáticamente
 */

package classifiers;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;

import org.w3c.dom.Attr;

import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.core.Attribute;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.converters.ConverterUtils.DataSource;
import weka.filters.Filter;
import weka.filters.unsupervised.attribute.StringToWordVector;

public class Model {
	private ArrayList<Attribute> bowAttSet;
	private Instances classify_stories_bow_set;
	
	private Instances training_stories;
	private Instances test_stories;
	protected Classifier cls;
	
	private static Instances bowSet;
	private static ArrayList<Attribute> bowInstanceAtts;

	protected Model(Classifier c) {
		cls = c;
		bowAttSet = new ArrayList<Attribute>();
		bowInstanceAtts = new ArrayList<Attribute>();
	}

	/*
	 * Al aplicar BoW debemos tener en cuenta que el archivo "bowizado" debe tener las mismas historias que se han 
	 * utilizado para calcular los features del set correspondiente (train, test o clasificación). Por tanto, debemos 
	 * tener 3 archivos BoW, uno para cada conjunto
	 * 
	 * También es necesario generar los archivos para el bag of words usando bowize (en ellos quitamos los corchetes, dejando sólo las 
	 * palabras sueltas) en el archivo de métricas
	 */
	
	private void readInstancesFile(String fileName) {
		try {
			DataSource source3 = new DataSource("/Users/ilaclaus/metricas/" + fileName + "_bow.arff");
			Instances prueba_bow = source3.getDataSet();

			if (prueba_bow.classIndex() == -1)
				prueba_bow.setClassIndex(prueba_bow.numAttributes() - 1);

			String[] options = new String[1];
			options[0] = "-L";                                    
			StringToWordVector bagOfWords = new StringToWordVector();                        
			bagOfWords.setOptions(options);
			bagOfWords.setInputFormat(prueba_bow);
			
			if (fileName.equalsIgnoreCase("Stories")) {
				classify_stories_bow_set = Filter.useFilter(prueba_bow, bagOfWords);

				bowAttSet = Collections.list(classify_stories_bow_set.enumerateAttributes());
			} else {
				bowSet = Filter.useFilter(prueba_bow, bagOfWords);
	
				bowInstanceAtts = Collections.list(bowSet.enumerateAttributes());
			}
		} catch (Exception e) {e.printStackTrace();}
	}
	
	private void setBowValues(Instances newDataSet, String fileName) {
		try {
			readInstancesFile(fileName);
			
			for (Attribute at : bowAttSet) {
				newDataSet.insertAttributeAt(at, 0);
				
				// Asigna el valor calculado para ese atributo a cada instancia
				for (int i = 0; i < bowSet.numInstances(); i++) { 
					if (bowInstanceAtts.contains(at))
						newDataSet.instance(i).setValue(0, bowSet.instance(i).value(at));
					
					else 
						newDataSet.instance(i).setValue(0, 0);
				}
			}
			
		} catch(Exception e) {e.printStackTrace();}
	}
	
	/*
	 * Obtenemos los atributos del bag of words
	 */
	private void bowAtts() {
		readInstancesFile("Stories");
	}
	
	private void setBowValues(Instances newDataSet) {
		try {
			for (Attribute at : bowAttSet) {
				newDataSet.insertAttributeAt(at, 0);

				// Asigna el valor calculado para ese atributo a cada instancia
				for (int i = 0; i < newDataSet.numInstances(); i++)
					newDataSet.instance(i).setValue(0, classify_stories_bow_set.instance(i).value(at));
			}

		} catch(Exception e) {e.printStackTrace();}
	}
	
	
	

	/*
	 * Los conjuntos de features se calculan a partir del archivo de métricas usando setFeatures, previo paso al entrenamiento del modelo.
	 */
	public void trainAndTest() {
		try {
			/* -------------------Instances------------------------- */
			DataSource source = new DataSource("/Users/ilaclaus/metricas/Stories_train_set_features.arff");
			training_stories = source.getDataSet();

			DataSource source2 = new DataSource("/Users/ilaclaus/metricas/Stories_test_set_features.arff");
			test_stories = source2.getDataSet();

			// setting class attribute if the data format does not provide this information
			// For example, the XRFF format saves the class attribute information as well
			if (training_stories.classIndex() == -1)
				training_stories.setClassIndex(training_stories.numAttributes() - 1);

			if (test_stories.classIndex() == -1)
				test_stories.setClassIndex(test_stories.numAttributes() - 1);

			/* APLICAR BAG OF WORDS A LOS CONJUNTOS */
			bowAtts();
			setBowValues(training_stories, "Stories_train_set");
			setBowValues(test_stories, "Stories_test_set");
			
			
			/* -------------------Training------------------------- */
			cls.buildClassifier(training_stories);

			/* -------------------Testing------------------------- */
			// Test the model
			Evaluation eTest = new Evaluation(training_stories);
			eTest.evaluateModel(cls, test_stories);

			// Print the result à la Weka explorer:
			String strSummary = eTest.toSummaryString();
			System.out.println(strSummary);

			// Get the confusion matrix
			double[][] cmMatrix = eTest.confusionMatrix();
		} catch (Exception e) {e.printStackTrace();} 
	}

	public void predict() {
		try {
			/* -------------------Use------------------------- */
			DataSource classify_source = new DataSource("/Users/ilaclaus/metricas/Stories_features.arff");
			Instances classify_stories = classify_source.getDataSet();

			// setting class attribute if the data format does not provide this information
			// For example, the XRFF format saves the class attribute information as well
			if (classify_stories.classIndex() == -1)
				classify_stories.setClassIndex(classify_stories.numAttributes() - 1);

			// APLICAR BOW AL CONJUNTO A CLASIFICAR
			setBowValues(classify_stories);

			double[] fDistribution;
			Instance ins = null;

			System.out.println("Resultados: ");
			System.out.println("-----------------------");

			for (int i = 0; i < classify_stories.numInstances(); i++) {
				// Get the likelihood of each classes
				// fDistribution[0] is the probability of being “positive”
				// fDistribution[1] is the probability of being “negative”

				ins = classify_stories.instance(i);
				ins.setDataset(training_stories);

				fDistribution = cls.distributionForInstance(ins);
				
				if (fDistribution[0] != 0) 
					System.out.println("Instancia: " + i + ", P[interesante] = " + fDistribution[0]);

			}
		} catch (Exception e) {e.printStackTrace();}
	}
}
