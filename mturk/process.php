<?php
require '../../mysql_login.php';
$db = new mysqli('localhost', $mysql_user, $mysql_pass, 'sharonl_patterns');

$data = $_POST;$workerid = $data["workerId"];$pid = $data["pid"];$best = $data["best"];$worst = $data["worst"];$screenWidth = $data["screenWidth"];$screenHeight = $data["screenHeight"];$colorDepth = $data["colorDepth"];$timeElapsed = $data["timeElapsed"];$rep = $data["rep"];

// put things into database 
$stmt = $db->prepare("INSERT INTO answers (workerId, pid,best,worst,screenWidth,screenHeight,colorDepth,timeElapsed,rep) VALUES (?,?,?,?,?,?,?,?,?)");
if(!$stmt) {
   echo $db->error;
} else {
   $stmt->bind_param("sissiiiii", $workerid,$pid,$best,$worst,$screenWidth,$screenHeight,$colorDepth,$timeElapsed,$rep);
   // execute
   $success = $stmt->execute();
   
   if (!$success)
   {
		echo "Error when inserting values";
		echo $db->error;
	}
   
   $stmt->close();
}


$db->close();

echo "Success!"

?>
