
<?php
require '../../mysql_login.php';
$db = new mysqli('localhost', $mysql_user, $mysql_pass, 'sharonl_patterns');
$workerid = $data["workerId"];
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
	}
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
