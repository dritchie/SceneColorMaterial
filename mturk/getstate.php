
<?php
require '../../mysql_login.php';
$db = new mysqli('localhost', $mysql_user, $mysql_pass, 'sharonl_patterns');$data = $_POST;
$workerid = $data["workerId"];$pid = -1;//get the number of questions done$questionsDone = -1;$stmt = $db->prepare("SELECT COUNT(pid) FROM answers WHERE workerId=?");if(!$stmt) {	  echo $db->error;} else {	 // bind params	 $stmt->bind_param("s", $workerid);	 // execute	 $stmt->execute();	   	 // bind results	 $stmt->bind_result($questionsDone);	   	 // fetch	 $stmt->fetch();	 $stmt->close();}//get the repetition question number if it exists, else add the worker to the assignments table and//note when he/she gets a repetition$repNumber = 0;$stmt = $db->prepare("SELECT repNumber FROM assignments WHERE workerId=?");if(!$stmt) {	  echo $db->error;} else {	 // bind params	 $stmt->bind_param("s", $workerid);	 // execute	 $stmt->execute();	 // bind results	 $stmt->bind_result($repNumber);	 // fetch	 $stmt->fetch();	 $stmt->close();}if (!$repNumber){	$randNumber = rand(3,5); //the replication can either be the third question up to the sixth question		$stmt = $db->prepare("INSERT INTO assignments (workerId,repNumber) VALUES (?,?)");	if(!$stmt) {		  echo $db->error;	} else {		 // bind params		 $stmt->bind_param("si", $workerid, $randNumber);		 // execute		 $stmt->execute();	   if (!$success)	   {			echo "Error when inserting values";			echo $db->error;		}		 $stmt->close();	}				//now retrieve the replication number	$stmt = $db->prepare("SELECT repNumber FROM assignments WHERE workerId=?");	if(!$stmt) {		  echo $db->error;	} else {		 // bind params		 $stmt->bind_param("s", $workerid);		 // execute		 $stmt->execute();		 // bind results		 $stmt->bind_result($repNumber);		 // fetch		 $stmt->fetch();		 $stmt->close();	}}//if the number of questions done matches the replication point, insert a replication$isRep = ($repNumber == ($questionsDone+1));if ($isRep){	$stmt = $db->prepare("SELECT pid FROM answers WHERE workerId=? ORDER BY RAND() LIMIT 1");	$isRep = 1;	if(!$stmt) {		  echo $db->error;	} else {		  // bind params		  $stmt->bind_param("s",$workerid);		  // execute		  $stmt->execute();		   		  // bind results		  $stmt->bind_result($pid);		   		  // fetch		  $stmt->fetch();		  $stmt->close();	}} else{
	// get the next relevant random question
	$stmt = $db->prepare("SELECT pid FROM patterns WHERE pid NOT IN (SELECT pid FROM answers WHERE workerId=?) ORDER BY RAND() LIMIT 1");
	if(!$stmt) {
		  echo $db->error;
	} else {
		  // bind params
		  $stmt->bind_param("s",$workerid);
		  // execute
		  $stmt->execute();
		   
		  // bind results
		  $stmt->bind_result($pid);
		   
		  // fetch
		  $stmt->fetch();
		  $stmt->close();
	}}
$questions = 6;
//$stmt = $db->prepare("SELECT COUNT(*) FROM patterns");
//if(!$stmt) {
//	  echo $db->error;
//} else {
	 // execute
//	 $stmt->execute();
	   
	 // bind results
//	 $stmt->bind_result($questions);
	   
	 // fetch
//	 $stmt->fetch();
//	 $stmt->close();
//}

$db->close();

echo $workerid. "," . $pid . "," . $questionsDone . "," . $questions . "," . $isRep;

?>

