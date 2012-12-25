package colorinference

import java.io.File

/**
 * Created with IntelliJ IDEA.
 * User: Daniel
 * Date: 12/24/12
 * Time: 12:31 PM
 * To change this template use File | Settings | File Templates.
 */

object WeightLearningTest
{
    val inputDir = "../PatternColorizer/out/mesh"

    def main(args:Array[String])
    {
        val files = new File(inputDir).listFiles.filter(_.getName.endsWith(".txt"))

        if (files.length == 0)
            throw new Error("No files found in the input directory!")

        // Setup model training parameters (we'll use Discrete color variables in this test)
        val params = new ModelTrainingParams
        {
            type VariableType = DiscreteColorVariable
            val colorVarParams = DiscreteColorVariableParams

//            initialLearningRate = 0.5
        }

        val meshes = for (f <- files) yield new SegmentMesh(params.colorVarParams.variableGenerator, f.getAbsolutePath)

        val model = ModelTraining(meshes, params)
    }
}
