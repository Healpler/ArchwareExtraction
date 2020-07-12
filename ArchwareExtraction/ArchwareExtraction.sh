#/bin/bash

archwarePath=`archwareExtractPath`
echo "You are redirecting to the program directory..."
cd $archwarePath
if [ $# -eq 0 ]
then
	echo "Type 'archwareExtract help' to get functions documentation."
else
	if [ $1 = "extractVA" ]
	then
		if [ $# -eq 2 ]
		then
			sudo Rscript $archwarePath/ExecutionProcesses.R loadPath $archwarePath
			sudo Rscript $archwarePath/ExecutionProcesses.R extractVA $2
		else
			echo "Wrong number of parameters for this function. Please type 'archwareExtraction help' to learn them."
		fi
	fi

	if [ $1 = "listAttackRemaining" ]
	then
		if [ $# -eq 1 ]
		then
			sudo Rscript $archwarePath/ExecutionProcesses.R loadPath $archwarePath
			sudo Rscript $archwarePath/ExecutionProcesses.R listAttackRemaining
		fi
	fi

	if [ $1 = "generateXML" ]
	then
		if [ $# -eq 1 ]
		then
			sudo Rscript $archwarePath/ExecutionProcesses.R loadPath $archwarePath
			sudo Rscript $archwarePath/ExecutionProcesses.R generateXML
		fi
	fi

	if [ $1 = "manageKeywords" ]
	then
		if [ $# -eq 4 -o $# -eq 2 ]
		then
			sudo Rscript $archwarePath/ExecutionProcesses.R loadPath $archwarePath
			sudo Rscript $archwarePath/ExecutionProcesses.R manageKeywords $2 $3 $4
		fi
	fi

	if [ $1 = "help" ]
	then
			echo "   ___  ______  _____  _   _ _    _  ___  ______ _____    ))    (("
			echo "  / _ \ | ___ \/  __ \| | | | |  | |/ _ \ | ___ \  ___|  //      \\\\ "
			echo " / /_\ \| |_/ /| /  \/| |_| | |  | / /_\ \| |_/ / |__   | \\\\____// |" 
			echo " |  _  ||    / | |    |  _  | |/\| |  _  ||    /|  __|  \~/ ~    ~\/~~/"
			echo " | | | || |\ \ | \__/\| | | \  /\  / | | || |\ \| |___   (|    _/o  ~~"
			echo " \_| |_/\_| \_| \____/\_| |_/\/  \/\_| |_/\_| \_\____/    /  /     ,|"
			echo "_________________________________________________________(~~~)__.-\ |"
			echo "_________________________________________________________ \`\`~~    | |"
			echo "_________________________________________________________  |      | |"
			sudo Rscript $archwarePath/ExecutionProcesses.R loadPath $archwarePath
			sudo Rscript $archwarePath/ExecutionProcesses.R help
	fi
fi
