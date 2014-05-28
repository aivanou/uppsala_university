-module (reverse_hash).
-export ([computeReverseHashes/6,job/8,spawnJobs/4,executeHashJob/5,solve/4]).


computeReverseHashes(_Fun,_Tree,ReverseImage,_Step,EndValue,HashValueList) when EndValue<ReverseImage -> HashValueList;
computeReverseHashes(Fun,Tree,ReverseImage,Step,EndValue,HashValueList) ->
	HashValue=Fun(ReverseImage),
	case sets:is_element(HashValue,Tree) of
		true -> computeReverseHashes(Fun,Tree,ReverseImage+Step,Step,EndValue,[{HashValue,ReverseImage}|HashValueList]);
		false -> computeReverseHashes(Fun,Tree,ReverseImage+Step,Step,EndValue,HashValueList)
	end.

job(ParentPid,StartValue,EndValue,_Step,_Tree,_Fun,_IterationStep,HashList) when EndValue<StartValue ->
	ParentPid ! {finish_job,dict:to_list(dict:from_list(HashList))},
	endSignal;
job(ParentPid,StartValue,EndValue,Step,Tree,Fun,IterationStep,HashList) ->
	HashValueList = computeReverseHashes(Fun,Tree,StartValue,Step,StartValue+IterationStep,[]),
	ParentPid ! {response,self()},
	receive 
		continue -> job(ParentPid,StartValue+IterationStep,EndValue,Step,Tree,Fun,IterationStep,lists:append(HashValueList,HashList));
		stop ->  ParentPid ! {finish_job,dict:to_list(dict:from_list(HashList))}
	end.

spawnJobs(0,_,_,_) -> [];
spawnJobs(Schedulers,Step,Fun,Tree) ->
	A=134217727, %2^27-1
	[spawn(reverse_hash,job,[self(),Schedulers,A,Step,Tree,Fun,10000,[]])|spawnJobs(Schedulers-1,Step,Fun,Tree)].

sendSignalToJobs([],_) -> none;
sendSignalToJobs([Pid|Rest],Signal) -> Pid!Signal, sendSignalToJobs(Rest,Signal).

executeHashJob(Pid,_,OutputHashTree,FinishedJobs,JobsAmount) when JobsAmount==FinishedJobs ->
	Pid!{reply,dict:to_list(OutputHashTree)},
	dict:to_list(OutputHashTree);
executeHashJob(Pid,JobList,OutputHashTree,FinishedJobs,JobsAmount) ->
	receive
 		{response,ChildPid} -> ChildPid!continue,
											 executeHashJob(Pid,JobList,OutputHashTree,FinishedJobs,JobsAmount);
		finish_up -> sendSignalToJobs(JobList,stop),
					 executeHashJob(Pid,JobList,OutputHashTree,FinishedJobs,JobsAmount);
					   
		{finish_job,HashValueList} -> executeHashJob(Pid,JobList,
			dict:merge(fun(_,V1,_) -> V1 end, OutputHashTree,dict:from_list(HashValueList)),
			FinishedJobs+1,JobsAmount)
	end.

solve(Fun,Inputs,Pid,Schedulers) ->
	HashTree = sets:from_list(Inputs),
	JobList=spawnJobs(Schedulers,Schedulers,Fun,HashTree),
	executeHashJob(Pid,JobList,dict:new(),0,Schedulers).

