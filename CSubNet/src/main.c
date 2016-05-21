#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include "coreDefs.h"
#include "matrix.h"
#include "neuralNetwork.h"
#include "trainingData.h"
#include "utils.h"
#include "csv.h"

#define MAX_NETWORK_SIZES (10)

void help() {
	PRINT_FLUSH(1,
		"CSubNet\n"
		"\n"
		"Usage:\n"
		"Help: CSubNet -h\n"
		"Train: CSubNet -t [OPTIONS] <training-data.csv> <input-columns> <output.net>\n"
		"Re-Train: CSubNet -r [OPTIONS] <old.net> <new-training-data.csv> <output.net>\n"
		"Execute: CSubNet -e [OPTIONS] <old.net> <new-data.csv> <output.csv>\n"
		"\n"
		"Options:\n"
		"Rounds: -r=n\n"
		"Layer Sizes: -s=v,w,x,y,z\n"
		"Label Columns: -l=x\n"
		"Algorithm: -a=[anneal,evolve,gradient]\n")
		system("pause");
}

int train(int paramCount, char** params) {
	char* trainCSVFile = NULL;
	int inputColumns;
	char* outputNetFile = NULL;
	int numSizes = 0;
	int labelColumns = 0;
	int layerSizes[MAX_NETWORK_SIZES];
	int defaultsFilled = 0;
	int alg = 0;
	intResult ir;

	int paramIdx;
	for (paramIdx = 0; paramIdx < paramCount; paramIdx++) {
		char* param = params[paramIdx];
		if (param[0] == '-') {
			size_t paramLen = strlen(param);
			switch (param[1]) {
			case 's':
				if (paramLen <= 2 || param[2] != '=') {
					PRINT_FLUSH(1, "Failed to parse option [%s].", param)
					help();
					return 1;
				}
				char* layerSizeString = param + 3;
				numSizes = parseCommaSeperatedInts(layerSizeString, layerSizes,
				MAX_NETWORK_SIZES);
				if (numSizes == -1) {
					PRINT_FLUSH(1, "Failed to parse layer sizes [%s].",
							layerSizeString)
					help();
					return 1;
				}
				break;
			case 'l':
				if (paramLen <= 2 || param[2] != '=') {
					PRINT_FLUSH(1, "Failed to parse option [%s].", param)
					help();
					return 1;
				}
				char* labelColumnsString = param + 3;
				ir = parseIntFromCString(labelColumnsString);
				if (!ir.ok) {
					PRINT_FLUSH(1, "Failed to parse string [%s] as an integer.",
							labelColumnsString)
					help();
					return 1;
				}
				labelColumns = ir.result;
				break;
			case 'a':
				if (paramLen <= 2 || param[2] != '=') {
					PRINT_FLUSH(1, "Failed to parse option [%s].", param)
					help();
					return 1;
				}
				char* algorithmString = param + 3;
				if (!strcmp(algorithmString, "anneal")) {
					alg = 0;
				} else if (!strcmp(algorithmString, "evolve")) {
					alg = 1;
				} else if (!strcmp(algorithmString, "gradient")) {
					alg = 2;
				} else {
					PRINT_FLUSH(1, "Failed to parse algorithm [%s].", param)
					help();
				}
				break;
			default:
				PRINT_FLUSH(1, "Unknown option [%s].", param)
				help();
				return 1;
			}
		} else {
			//we've passed the options
			if (paramCount - paramIdx != 3) {
				help();
				return 1;
			}
			trainCSVFile = params[paramIdx];
			ir = parseIntFromCString(params[paramIdx + 1]);
			if (!ir.ok) {
				PRINT_FLUSH(1, "Failed to parse string [%s] as an integer.",
						params[1])
				help();
				return 1;
			}
			inputColumns = ir.result;
			outputNetFile = params[paramIdx + 2];
			defaultsFilled = 1;
			break;
		}
	}
	if (!defaultsFilled) {
		help();
		return 1;
	}

	trainingData trainData, *trainDataPtr;
	trainDataPtr = &trainData;
	if (readTrainingData(trainDataPtr, trainCSVFile, inputColumns,
			labelColumns) == NULL) {
		PRINT_FLUSH(1, "An error occurred while reading the CSV.\n")
		help();
		return 1;
	}

	neuralNetwork* network;
	if (numSizes == 0) {
		network = createStandardNetwork(getTrainingDataIntputs(trainDataPtr),
				getTrainingDataOutputs(trainDataPtr));
	} else {
		if ((getTrainingDataIntputs(trainDataPtr) != layerSizes[0])
				|| (getTrainingDataOutputs(trainDataPtr) != layerSizes[numSizes - 1])) {
			PRINT_FLUSH(1,
					"The layer sizes provided do not match the dimensions of the CSV.\n")
			clearTrainingData(trainDataPtr);
			help();
			return 1;
		}
		network = createSizedNetwork(layerSizes, numSizes - 1);
	}

	neuralNetwork* optimalNetwork = NULL;
	if (alg == 0) {
		optimalNetwork = annealNetwork(network, trainDataPtr);
	} else if (alg == 1) {
		optimalNetwork = evolveNetwork(network, trainDataPtr);
	} else if (alg == 2) {
		optimalNetwork = gradientClimbNetwork(network, trainDataPtr);
	}

	stringFragment encoded = encodeNeuralNetwork(optimalNetwork);
	int errCode = 0;
	if (!writeFileFromMemory(outputNetFile, encoded)) {
		PRINT_FLUSH(1, "An error occurred while saving the network to [%s].\n",
				outputNetFile)
		errCode = 1;
	}

	free(encoded.start);
	deleteNetwork(optimalNetwork);
	deleteNetwork(network);
	clearTrainingData(trainDataPtr);
	return errCode;
}

int reTrain(int paramCount, char** params) {
	char* inputNetFile = NULL;
	char* trainCSVFile = NULL;
	char* outputNetFile = NULL;
	int defaultsFilled = 0;
	int labelColumns = 0;
	int alg = 0;
	int paramIdx;
	intResult ir;
	for (paramIdx = 0; paramIdx < paramCount; paramIdx++) {
		char* param = params[paramIdx];
		if (param[0] == '-') {
			size_t paramLen = strlen(param);
			switch (param[1]) {
			case 'l':
				if (paramLen <= 2 || param[2] != '=') {
					PRINT_FLUSH(1, "Failed to parse option [%s].\n", param)
					help();
					return 1;
				}
				char* labelColumnsString = param + 3;
				ir = parseIntFromCString(labelColumnsString);
				if (!ir.ok) {
					PRINT_FLUSH(1, "Failed to parse string [%s] as an integer.\n",
							labelColumnsString)
					help();
					return 1;
				}
				labelColumns = ir.result;
				break;
			case 'a':
				if (paramLen <= 2 || param[2] != '=') {
					PRINT_FLUSH(1, "Failed to parse option [%s].", param)
						help();
					return 1;
				}
				char* algorithmString = param + 3;
				if (!strcmp(algorithmString, "anneal")) {
					alg = 0;
				} else if (!strcmp(algorithmString, "evolve")) {
					alg = 1;
				} else if (!strcmp(algorithmString, "gradient")) {
					alg = 2;
				} else {
					PRINT_FLUSH(1, "Failed to parse algorithm [%s].", param)
						help();
				}
				break;
			default:
				PRINT_FLUSH(1, "Unknown option [%s].\n", param)
				help();
				return 1;
			}
		} else {
			//we've passed the options
			if (paramCount - paramIdx != 3) {
				help();
				return 1;
			}
			inputNetFile = params[paramIdx];
			trainCSVFile = params[paramIdx + 1];
			outputNetFile = params[paramIdx + 2];
			defaultsFilled = 1;
			break;
		}
	}
	if (!defaultsFilled) {
		help();
		return 1;
	}

	neuralNetwork* network = readNeuralNetwork(inputNetFile);
	if (!network) {
		PRINT_FLUSH(1, "An error occurred while loading the network from [%s].",
				outputNetFile)
		help();
		return 1;
	}

	trainingData trainData, *trainDataPtr;
	trainDataPtr = &trainData;
	if (readTrainingData(trainDataPtr, trainCSVFile, getNetworkInputs(network),
			labelColumns) == NULL) {
		deleteNetwork(network);
		PRINT_FLUSH(1, "An error occurred while reading the CSV.")
		help();
		return 1;
	}

	neuralNetwork* optimalNetwork = NULL;
	if (alg == 0) {
		optimalNetwork = annealNetwork(network, trainDataPtr);
	} else if (alg == 1) {
		optimalNetwork = evolveNetwork(network, trainDataPtr);
	} else if (alg == 2) {
		optimalNetwork = gradientClimbNetwork(network, trainDataPtr);
	}


	stringFragment encoded = encodeNeuralNetwork(optimalNetwork);
	int errCode = 0;
	if (!writeFileFromMemory(outputNetFile, encoded)) {
		PRINT_FLUSH(1, "An error occurred while saving the network to [%s].",
				outputNetFile)
		errCode = 1;
	}

	free(encoded.start);
	deleteNetwork(optimalNetwork);
	deleteNetwork(network);
	clearTrainingData(trainDataPtr);
	return errCode;
}

int execute(int paramCount, char** params) {
	int defaultsFilled = 0;
	char* netFile = NULL;
	char* inputCSVFile = NULL;
	char* outputCSVFile = NULL;

	int labelColumns = 0;
	int paramIdx;
	intResult ir;
	for (paramIdx = 0; paramIdx < paramCount; paramIdx++) {
		char* param = params[paramIdx];
		if (param[0] == '-') {
			size_t paramLen = strlen(param);
			switch (param[1]) {
			case 'l':
				if (paramLen <= 2 || param[2] != '=') {
					PRINT_FLUSH(1, "Failed to parse option [%s].", param)
					help();
					return 1;
				}
				char* labelColumnsString = param + 3;
				ir = parseIntFromCString(labelColumnsString);
				if (!ir.ok) {
					PRINT_FLUSH(1, "Failed to parse string [%s] as an integer.",
							labelColumnsString)
					help();
					return 1;
				}
				labelColumns = ir.result;
				break;
			default:
				PRINT_FLUSH(1, "Unknown option [%s].", param)
				help();
				return 1;
			}
		} else {
			//we've passed the options
			if (paramCount - paramIdx != 3) {
				help();
				return 1;
			}
			netFile = params[paramIdx];
			inputCSVFile = params[paramIdx + 1];
			outputCSVFile = params[paramIdx + 2];
			defaultsFilled = 1;
			break;
		}
	}

	if (!defaultsFilled) {
		help();
		return 1;
	}

	neuralNetwork* network = readNeuralNetwork(netFile);
	if (!network) {
		PRINT_FLUSH(1, "An error occurred while loading the network from [%s].",
				netFile)
		help();
		return 1;
	}

	csv* csvData = readCSV(inputCSVFile);
	if (!csvData) {
		deleteNetwork(network);
		PRINT_FLUSH(1, "An error occurred while reading the CSV.")
		help();
		return 1;
	}
	int networkInputs = getNetworkInputs(network);
	matrix* mat = extractMatrixFromCSV(csvData, 0, labelColumns, networkInputs,
			csvData->rows);
	if (!mat) {
		deleteCSV(csvData);
		PRINT_FLUSH(1, "An error occurred while reading the CSV.")
		help();
		return 1;
	}

	matrix* out = applyNetwork(network, mat, NULL, NULL);
	deleteNetwork(network);

	matrix* augmented = agumentMatrix(mat, out, NULL);
	deleteMatrix(mat);
	deleteMatrix(out);

	int errCode = 0;
	//writeMatrixCSV(stdout, augmented, csvData, labelColumns, networkInputs, out->width);
	if (!writeMatrixAsCSVToFile(outputCSVFile, augmented, csvData,
			labelColumns, networkInputs, out->width)) {
		errCode = 1;
		PRINT_FLUSH(1, "An error occurred while saving the results to [%s].",
				outputCSVFile)
	}

	deleteMatrix(augmented);
	deleteCSV(csvData);
	return errCode;
}

int main(int argc, char *argv[]) {
	int n;
	for (n = 0; n < argc; n++) {
		PRINT_FLUSH(1, "Argument %d [%s]\n", n, argv[n])
	}

	if (argc < 2) {
		help();
		return 1;
	}
	char* command = argv[1];
	if (strcmp(command, "-t") == 0) {
		return train(argc - 2, &argv[2]);
	} else if (strcmp(command, "-r") == 0) {
		return reTrain(argc - 2, &argv[2]);
	} else if (strcmp(command, "-e") == 0) {
		return execute(argc - 2, &argv[2]);
	} else {
		help();
		if (strcmp(command, "-h") == 0) {
			return 0;
		}
		return 1;
	}
}
