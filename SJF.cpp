/////////////////////////////////////////////////////////////////
//
//	Jason Hoang, Taylor Condrack, Samuel McGregor - CSCE 4600 - Keathly
//	2/23/2016
//  Project 1
//  This program simulates the shortest job first scheduling algorithm
//  The algorithm runs on a simulation of a unicore and quadcore processor
/////////////////////////////////////////////////////////////////

#include <cstdlib>
#include <cmath>
#include <ctime>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <iostream>
#include <iomanip>

using namespace std;

class Process{			//This class serves as the data structure containing the process 3-tuple
	private:
		int processID;
		bool procDone;
		int numCycles;
		int memSize;
		int coreNum;

	public:
		Process();			//constructor
		Process(int, int, int, int);		//overloaded constructor
		double randDist(int, int, double, double);	//the normal distribution algorithm
		void addProcess(vector<Process>&, int, int, int, int);	//add new process with stats to list
		int totalWait;	//made public to be used for function calulations
		void waitTime(vector<Process>&, int, int, int);//calculates waittime for each process on a single processor
		void waitTime2(vector<Process>&,int,int,int);//calculates waittime for each process on multi core processor
		int getNumCycles() const;
		int sjf(vector<Process>&, int);//sjf algorithm for single processor
		void display(vector<Process>&,vector<Process>&,int);//prints process data
		int multicoreSjf(vector<Process>&,int);

};

//Constructor
Process::Process(){
	int numCycles = 0;
	bool procDone = false;
	int totalWait = 0;
}

//Overloaded Constructor
Process::Process(int newProcessID, int newNumCycles, int newMemSize, int newCoreNum){
	processID = newProcessID;
    	numCycles = newNumCycles;
    	memSize = newMemSize;
	coreNum = newCoreNum;
	totalWait = 0;
	procDone = false;
}

int Process::getNumCycles() const{
	return numCycles;
}

//This is the normal distribution algorithm that I used  to generate random numbers.
//The normal distribution utilizes the Box Muller Transformation to generate the random numbers
double Process::randDist(int mean, int stddev, double min, double max) {
	static double n2 = 0.0;		//these variables are used to generate the normally distributed
     	static int n2_cached = 0;	//faster by keeping the 2nd random number generated from the previous
					//instance of the function
        if (!n2_cached) {
		double x, x1, x2, y, y1, y2, r;
 		do {	//generate 2 random numbers within the specified range
 			x1 = ((double)rand()/(double)(RAND_MAX));
			x2 = (max - min);
			x = fmod(x1,x2);

 	    		y1 = ((double)rand()/(double)(RAND_MAX));
			y2 = (max - min);
			y = fmod(y1,y2);

 	    		r = x*x + y*y;	//begin to normally distribute the randomly generated numbers
 		}while (r == 0.0 || r > 1.0);
        	double diff = max - min;
        	double d = sqrt(abs(-2.0*log(r)/r));		//Box Muller Transformation algorithm
		double n1 = x*d;
 		n2 = y*d;		//store the 2nd generated number normally distributed
		double result = n1*mean*stddev + (min*4);
        	while((result < min)||(result > max))
		{	//error correcting
			if (result < min)
			{
				result += min;
			}
			else if (result > max)
			{
				result -= max;
			}
		}
		n2_cached = 1;		//set the if flip flop
        	return result;	//return the 1st normally distributed random number
  	}
	else
	{
        	n2_cached = 0;			//set the if flip flop
		double result = n2*mean*stddev + (min*4);
		while((result < min)||(result > max))
                {	//error correcting
                        if (result < min)
                        {
                                result += min;
                        }
                        else if (result > max)
                        {
                                result -= max;
                        }
                }
		return result;	//return the 2nd normally distributed random number
	}
}

//This function adds the newly distributed process with its stats to the vector data structure
void Process::addProcess(vector<Process>& plist, int newProcessID, int newNumCycles, int newMemSize, int newCoreNum){
	Process newProcess(newProcessID, newNumCycles, newMemSize, newCoreNum);	//create the new process
	plist.push_back(newProcess);				//store the process
}

int Process::sjf(vector<Process>& plist, int numProc)
{
    int i;
    int start;
    int procComplete = 0;
    int loopCount = 0;
    int penaltyTime = 0;
    Process temp;
    int curr=0;

//this ensures that the first process to arrive is scheduled first
//it check is the other processes have arrived before the first process finishes
//if not it will schedule the second process that arrived
    if(plist[0].numCycles>2450)
        start=1;
    else if(plist[0].numCycles<=2450)
    {
        if((plist[1].numCycles+plist[0].numCycles)>=2450)
            start=2;
        else
            start=3;
    }
//this loop sorts the rest of the arrived processes in ascending order of number of cycles
//quicksort
    for(int k=start;k<numProc;k++)
        {
            curr = k;
            for(int j=k+1;j<numProc;j++)
            {
                if(plist[j].numCycles<plist[curr].numCycles)
                    curr=j;
            }
            temp=plist[k];
            plist[k]=plist[curr];
            plist[curr]=temp;
        }

//calculates wait time and penalty time of context switches for each process
    while (procComplete != numProc)
    {
        for(i=0;i<numProc;i++)
        {
            plist[i].procDone = true;
            procComplete++;
            waitTime(plist,i,numProc, 0);
            penaltyTime+=10;
        }

	}

	plist[i].totalWait -= 10;	//subtract the extra context switch time
	penaltyTime -= 10;
	return penaltyTime;

}

int Process::multicoreSjf(vector<Process>& plist, int numProc)
{
    int i;
    int start,start1,start2,start3;
    int core0=0,core1=0,core2=0,core3=0;
    int procComplete = 0;
    int loopCount = 0;
    int penaltyTime = 0;
    Process temp;
    int curr=0;

//schedules the first 4 processes that arrived to be scheduled to the 4 cores
//checks the next process that is scheduled to each core to see if the rest of the processes have arrived
//if they havent schedules the next to arrive
//after the processes have arrived the rest can be sorted then assigned to each core
        if(plist[0].numCycles>2450&&plist[1].numCycles>2450&&plist[2].numCycles>2450&&plist[3].numCycles>2450)
        {
            start=1;
            start1=1;
            start2=1;
            start3=1;
        }
        else if(plist[0].numCycles+plist[4].numCycles>=2450)
        {
            start=2;
        }
        else if(plist[0].numCycles+plist[4].numCycles<2450)
        {
            start=3;
        }
        else if(plist[1].numCycles+plist[5].numCycles>=2450)
        {
            start1=2;
        }
        else if(plist[1].numCycles+plist[5].numCycles<2450)
        {
            start1=3;
        }
        else if(plist[2].numCycles+plist[6].numCycles>=2450)
        {
            start2=2;
        }
        else if(plist[2].numCycles+plist[6].numCycles<2450)
        {
            start2=3;
        }
        else if(plist[3].numCycles+plist[7].numCycles>=2450)
        {
            start3=2;
        }
        else if(plist[3].numCycles+plist[7].numCycles<2450)
        {
            start3=3;
        }


//sorts the processes assigned to each core independently per core
    for(int k=start;k<numProc;k++)
    {
        if(plist[k].coreNum==0)
        {
            curr = k;
            for(int j=k+1;j<numProc;j++)
            {
                if(plist[j].numCycles<plist[curr].numCycles)
                    curr=j;
            }
            temp=plist[k];
            plist[k]=plist[curr];
            plist[curr]=temp;
            core0++;
        }
    }

    for(int k=start;k<numProc;k++)
    {
        if(plist[k].coreNum==1)
        {
            curr = k;
            for(int j=k+1;j<numProc;j++)
            {
                if(plist[j].numCycles<plist[curr].numCycles)
                    curr=j;
            }
            temp=plist[k];
            plist[k]=plist[curr];
            plist[curr]=temp;
            core1++;
        }
    }

    for(int k=start;k<numProc;k++)
    {
        if(plist[k].coreNum==2)
        {
            curr = k;
            for(int j=k+1;j<numProc;j++)
            {
                if(plist[j].numCycles<plist[curr].numCycles)
                    curr=j;
            }
            temp=plist[k];
            plist[k]=plist[curr];
            plist[curr]=temp;
            core2++;
        }
    }

    for(int k=start;k<numProc;k++)
    {
        if(plist[k].coreNum==3)
        {
            curr = k;
            for(int j=k+1;j<numProc;j++)
            {
                if(plist[j].numCycles<plist[curr].numCycles)
                    curr=j;
            }
            temp=plist[k];
            plist[k]=plist[curr];
            plist[curr]=temp;
            core3++;
        }
    }


//calculates the waittime and penalty time for the processes in each core
        for(i=0;i<numProc;i++)
        {
            if(plist[i].coreNum==0)
            {
                plist[i].procDone = true;
                procComplete++;
                waitTime2(plist,i,numProc, 0);
                penaltyTime+=10;
            }
            else if(plist[i].coreNum==1)
            {
                plist[i].procDone = true;
                procComplete++;
                waitTime2(plist,i,numProc, 0);
                penaltyTime+=10;
            }
            else if(plist[i].coreNum==2)
            {
                plist[i].procDone = true;
                procComplete++;
                waitTime2(plist,i,numProc, 0);
                penaltyTime+=10;
            }
            else if(plist[i].coreNum==3)
            {
                plist[i].procDone = true;
                procComplete++;
                waitTime2(plist,i,numProc, 0);
                penaltyTime+=10;
            }


	}
//penalty time is 40 less because the first process on each core
	plist[i].totalWait -= 40;	//subtract the extra context switch time
	penaltyTime -= 40;
	return penaltyTime;

}
//calculates wait time
void Process::waitTime2(vector<Process>& plist, int index, int numProc, int overflow)
{
    int i=index-1;
    bool a=true;
//waittime for each cores first process
    if(index==0)
        plist[index].totalWait=0;
    else if(index==1)
        plist[index].totalWait=50;
    else if(index==2)
        plist[index].totalWait=100;
    else if(index==3)
        plist[index].totalWait=150;
    else
    {
        while(a==true&&i!=3)
        {
            if(plist[i].coreNum==plist[index].coreNum)
            {
                plist[index].totalWait=plist[i].numCycles+plist[i].totalWait;
                a=false;

            }
            else
                i--;
        }
    }
}

void Process::waitTime(vector<Process>& plist, int index, int numProc, int overflow)
{
    if(index==0)
    {
        plist[index].totalWait=0;
    }
    else
        plist[index].totalWait=plist[index-1].numCycles+plist[index-1].totalWait;

}
//prints the process data
void Process::display(vector<Process>& plist,vector<Process>& plist2,int numProc)
{
    cout<<endl<<endl;
    cout<<"Data is printed in the order that the processes are executed"<<endl<<endl;
    cout<<"***************SINGLE PROCESSOR DATA***************"<<endl;
    cout<<setw(10)<<"Process ID   |   Number of Cycles   |    Mem Size"<<endl;
    for(int q=0;q<numProc;q++)
    {
        cout<<setw(5)<<plist[q].processID<<setw(21)<<plist[q].numCycles<<setw(21)<<plist[q].memSize<<endl;
    }

    cout<<endl<<endl;

    cout<<"*************************MULTI-CORE PROCESSOR DATA*************************"<<endl;
    cout<<setw(10)<<"Process ID   |   Number of Cycles   |    Mem Size     |    Assigned Core Number"<<endl;
    for(int q=0;q<numProc;q++)
    {
        cout<<setw(5)<<plist2[q].processID<<setw(21)<<plist2[q].numCycles<<setw(21)<<plist2[q].memSize<<setw(20)<<plist2[q].coreNum<<endl;
    }
}
//INT MAIN
int main() {
	Process process;	//Process 3-tuple class
	vector<Process> plist;	//vector data structure
	vector<Process> plist2;
	int numProc = 50;	//number of processes
	int numCyclesList[numProc];
	int memSizeList[numProc];
    	int k, i, j, x;	//counters
	int cycleAvg = 0;
	int memAvg = 0;
	int avgWait = 0;
	int avgWait2 =0;
	int penaltyTime = 0;
	int penaltyTime2 = 0;
	cout << "\nThis program will create " << numProc << " processes, each with an ID #, the number of cycles to complete the process, and the size of the memory footprint." << endl;

     	srand(time(NULL));	//seed the rand function
     	for(k=0; k<numProc; k++) {	//generates randomly distributed numbers for the number of cycles
        	double val = process.randDist(6000, 1, 1000.0, 11000.0);
         	int i = (int)floor(val + 0.5); //rounds the number genreated
         	if (i >= 1000 && i <= 11000)
             	{
			numCyclesList[k] = i;	//store in temporary list
			cycleAvg += i;		//calculate average
			//cout << "numCycles: " << i << endl;
		}
     	}
	cout << "The required # of cycles to complete each process has been generated for each process." << endl;

	for(k=0; k<numProc; k++) {	//generates randomly distributed numbers for the memory sizes
                double val2 = process.randDist(20480, 1, 1024.0, 102400.0);
                int j = (int)floor(val2 - 0.5);	//rounds the number genreated
                if (j >= 1024 && j <= 102400)
                {
                        memSizeList[k] = j;	//store in temporary list
			memAvg += j;		//calculate average
                }
        }
	cout << "The the memory size for each process has been generated for each process." << endl;

	cycleAvg /= numProc;	//calculate averages
	memAvg /= numProc;

	int coreNum = 4;
	for(k=0; k<numProc; k++)
    {	//add each process to the vector data structure
			process.addProcess(plist, k, numCyclesList[k], memSizeList[k], coreNum%4);
			coreNum++;
	}

    plist2=plist;

	penaltyTime = process.sjf(plist, numProc);

    penaltyTime2 = process.multicoreSjf(plist2,numProc);

//calculates the averages
	for(x=0; x<numProc; x++)
	{
		avgWait += plist[x].totalWait;
	}
	avgWait /= numProc;

    for(x=0; x<numProc; x++)
	{
		avgWait2 += plist2[x].totalWait;
	}
	avgWait2 /= numProc;

    process.display(plist,plist2,numProc);
	cout << endl;
	cout<<"*****Single Processor Data*****"<<endl;
	cout << "Average number of cycles = " << cycleAvg << endl;	//print average outputs
	cout << "Average size of memory footprint = " << memAvg << endl;
	cout << "Average waiting time = " << avgWait << endl;
	cout << "Total Penalty Time = " << penaltyTime << endl;
	cout << endl;


	cout<<"*****Multi-core Processor Data*****"<<endl;
	cout << "Average number of cycles = " << cycleAvg << endl;	//print average outputs
	cout << "Average size of memory footprint = " << memAvg << endl;
	cout << "Average waiting time = " << avgWait2 << endl;
	cout << "Total Penalty Time = " << penaltyTime2 << endl;
	cout << endl;




	return 0;
}
