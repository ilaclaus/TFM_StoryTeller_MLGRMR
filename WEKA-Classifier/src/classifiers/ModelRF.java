package classifiers;
import weka.classifiers.trees.RandomForest;

public class ModelRF extends Model {
	public ModelRF() {
		super(new RandomForest());
	}

}
