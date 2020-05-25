#!/bin/bash
#create alias
#refresh bashrc
#Installation
read -p "Where do you want to install the extraction program ?" rep

sudo echo "alias archwareExtract='source $rep/ArchwareExtraction/ArchwareExtraction.sh'" >> ~/.bashrc
sudo echo "alias archwareExtractPath='echo $rep/ArchwareExtraction/'" >> ~/.bashrc

if [ -d $rep/ArchwareExtraction ]
then
	sudo rm -rf $rep/ArchwareExtraction
fi
sudo cp -r ../ArchwareExtraction $rep
sudo chmod -R 755 $rep/ArchwareExtraction
source ~/.bashrc

#Install packages that R needed to install its own packages.
os_selection=("Ubuntu" "Debian" "Others" "Fedora" "CentOS" "RHEL" "Solaris")
select os in "${os_selection[@]}"
do
	case $os in
		"Ubuntu"|"Debian"|"Others")
			echo "You'v chose $os ."
			sudo apt-get install libcurl4-openssl-dev
			sudo apt-get install libxml2-dev
			sudo apt-get install libpoppler-cpp-dev
			sudo apt-get install libssl-dev
			sudo apt update
			sudo apt upgrade
			;;
		"Fedora"|"CentOS"|"RHEL")
			echo "You'v chose $os ."
			sudo yum install libcurl4-devel
			sudo yum install libxml2-devel
			sudo yum install openssl-devel
			;;
		"Solaris")
			echo "You'v chose $os ."
			pkgadd -d http://get.opencsw.org/now
			/opt/csw/bin/pkgutil -U
			/opt/csw/bin/pkgutil -y -i libcurl_dev
			#May missed some packages
			;;
		*)
			echo "ArchwareExtraction Installation warning : Invalid entry, please get the number which references your OS."
			;;
	esac
	break
done
echo "Installation complete. Type 'archwareExtraction help' command to start."
