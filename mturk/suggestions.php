<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<?php

$showsample = ($_GET['assignmentId'] == "ASSIGNMENT_ID_NOT_AVAILABLE"); 
$workerId = $_GET['workerId'];

?>



<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Pattern Coloring Suggestions</title>
<script type="text/javascript" src="jquery-1.8.3.min.js"></script>
<script type="text/javascript" src="jquery-ui-1.9.2.custom.min.js"></script>

<script type="text/javascript">
//Mturk stuff

var previewing = false;

function populateAssignmentID(field_id) {
  document.getElementById("sw_field").value = screen.width;	
  document.getElementById("sh_field").value = screen.height;	
  document.getElementById("cd_field").value = screen.colorDepth;
  document.getElementById("ua_field").value = navigator.userAgent + "||" + navigator.vendor;
	
  var assignment_id_field = document.getElementById(field_id);
  var paramstr = window.location.search.substring(1);
  var parampairs = paramstr.split("&");
  for (i in parampairs) {
	var pair = parampairs[i].split("=");
	if (pair[0] == "assignmentId") {
	  if (pair[1] == "ASSIGNMENT_ID_NOT_AVAILABLE") {
	  	previewing = true;
		$("#previewnotice").html("<p><b>You are previewing this HIT.</b>  To perform this HIT, please accept it. </p>");
	  } else {
		assignment_id_field.value = pair[1];
	  }
	  return;
	}
  }
}


//history log
var logger = []; //contains timestamp and action
var exportColors = [];



</script>

<style type="text/css">


body {
	font-family: Arial, sans serif;
	color:#333333;
	font-size: 11pt;
}

#previewnotice {
	color:#CC0000;
}

#progress {
	float: right;
	font-size: 14pt;
}

.highlighted {
	border-style:solid;
	border-color:#333333;
	border-width:thin;
}

.selected {
	border-style:solid;
	border-color:#333333;
	border-width:thin;
}

.selectable {
	cursor: pointer;
}

.unselectable {
	cursor: auto;
	background-color: #CCCCCC;
}


h2 {
	font-family:Arial, Helvetica, sans-serif;
}

h3 {
	font-family:Arial, Helvetica, sans-serif;
}


.delete {
	position: relative;
	top: -110px;
	left:-20px;
	cursor: pointer;
	width:20px;
	height:20px;
	z-index:1;
}

#thankyou {
	text-align: center;
	display: none;
}

#finish_button {
	font-size: 14pt;
}

#sorry {
	text-align: center;
	display: none;
}

#interface {
	width: 900px;
	margin-left: auto;
	margin-right: auto;
}

#grid {
	width: 590px;
	clear: both;
}

#target
{	
	width:130px;
	float:left;
}
#target img
{	
	margin: 5px;
}

#suggestionGrid
{
	float:left;
	width: 590px;
}

#display
{
	clear: both;
}

#answers
{
	float: left;
	clear: right;
}

#next_button
{
	margin: 5px;
	float:right;
	clear:left;
}


.answeritem {
	margin: 5px;
	float: left;
	cursor: auto;
}


.suggestion {
	padding: 5px;
	float: left;
}

.simage{
	width: 130px;
	height: 130px;
}

.answerbox
{
	width: 300px;
	min-height: 270px;
	margin-top: 10px;
}

.placeholder
{
	clear:both;
}




</style>




<script type="text/javascript">

var srcUrl = "";
var userId = -1;
var numStates = 7;
var curState = 0;
var paletteString = "";
var pid = -1;
var start = Date.now();
var isRep = -1;



function registerEvents()
{
	//create the droppables
	$("#best4").droppable({
		accept: ":not(.ui-sortable-helper)",
		drop: function(event, ui){
		
			$(this).find(".placeholder").remove()
			var dragged = ui.draggable;
			dragged.removeClass("selectable").addClass("unselectable");
					
			//clone the dragged
			var clone = dragged.clone()
			var id = dragged.attr("id").replace("s_","")
			clone.attr("id", id)	
			clone.removeClass("suggestion").removeClass("unselectable").addClass("answeritem")
			clone.appendTo(this)
			
			//add a delete button
			var x = "<img src='x.png' class='delete' />"
			$("#"+id).append(x)
			$("#"+id).mouseover(function(){$(this).find(".delete").show()});
			$("#"+id).mouseout(function(){$(this).find(".delete").hide()});
			$("#"+id).find(".delete").click(function(){
				//enable the draggable again
				$("#s_"+id).draggable("option","cancel","");
				$("#s_"+id).css("opacity","1.0");
				$("#s_"+id).removeClass("unselectable").addClass("selectable");
				//remove
				$("#"+id).remove()
			});
			
			
			$(this).append("<div class='placeholder'></div>");
			

			
			//disable the current draggable, and reduce opacity
			dragged.draggable("option","cancel","#grid");
			dragged.css("opacity","0.1");

		}
	})
	$("#worst4").droppable({
		accept: ":not(.ui-sortable-helper)",
		drop: function(event, ui){
			$(this).find(".placeholder").remove()
			var dragged = ui.draggable;
			dragged.removeClass("selectable").addClass("unselectable");
					
			//clone the dragged
			var clone = dragged.clone()
			var id = dragged.attr("id").replace("s_","")
			clone.attr("id", id)	
			clone.removeClass("suggestion").removeClass("unselectable").addClass("answeritem")
			clone.appendTo(this)
			
			//add a delete button
			var x = "<img src='x.png' class='delete' />"
			$("#"+id).append(x)
			$("#"+id).mouseover(function(){$(this).find(".delete").show()});
			$("#"+id).mouseout(function(){$(this).find(".delete").hide()});
			$("#"+id).find(".delete").click(function(){
				//enable the draggable again
				$("#s_"+id).draggable("option","cancel","");
				$("#s_"+id).css("opacity","1.0");
				$("#s_"+id).removeClass("unselectable").addClass("selectable");
				//remove
				$("#"+id).remove()
			});
			
			
			$(this).append("<div class='placeholder'></div>");
			

			
			//disable the current draggable
			dragged.draggable("option","cancel","#grid");
			dragged.css("opacity","0.1");
		}
	})
	
	
	//register events
	$("#next_button").click(onNextClick);
}

function getState()
{
	//mturk stuff
	populateAssignmentID("myAssignmentId");
	
	var wid = <?php 
		if ($showsample) {
			echo -1;
		} else {
			echo '"'.$workerId.'"'; 
		}
	?>;
	//Get the state then load the page
	//this calls the onLoad function
	
	if (previewing)
	{
		//set a default pid	
		pid = 786134;
		curState = 0;
		numStates = 6;
		onLoad();
		
	} else 
	{
		$.post("getstate.php", {workerId: wid}, function(data) {
				userId = data.split(',')[0].replace(/\r?\n|\r/g,"");
				pid = parseInt(data.split(',')[1])
				curState = parseInt(data.split(',')[2]);
				numStates = parseInt(data.split(',')[3]);
				isRep = parseInt(data.split(',')[4]);
				onLoad();
			});
	}
	
}


function onLoad()
{

	if (curState < numStates)
	{
		updateProgress()
		loadGrid()
		registerEvents()
		start = Date.now()
	} else
	{
		$("#thankyou").show()
		$("#interface").hide()
	}
	
}

function shuffle(o){ //v1.0
    for(var j, x, i = o.length; i; j = parseInt(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x);
    return o;
};

function loadGrid()
{
	console.log("Loading grid")
	//draw the target image
	//$("#target").append("<img src='images/"+pid+"_t.png' class='simage' /></div>");
	
	
	//load the grid with the given pattern id
	var suffixes = ["a","m","c","r"]
	
	//shuffle all the pattern options
	var options = []
	for (var i=0; i<suffixes.length; i++)
	{
		for (var j=0; j<4; j++)
		{
			options.push(pid+"_"+suffixes[i]+j)
		}	
	}
	shuffle(options)

	//draw the grid, TODO: first just draw it with placeholder images
	for (var i=0; i<options.length; i++)
	{
		var suggId = "s_"+options[i]
		$("#grid").append("<div class='suggestion selectable' id='"+suggId+"'><img src='images/"+options[i]+".png' class='simage' /></div>")
		
		//make the suggestion draggable
		$("#"+suggId).draggable({helper: "clone"})
		
		
		
	}


}


function onNextClick(e)
{
	if (previewing)
	{
		alert("You are previewing this HIT. Please accept it to continue");
		return false;
	}

	//validate the number of answers
	var best4 = $("#best4").find(".answeritem")
	var worst4 = $("#worst4").find(".answeritem")
	if (best4.length != 4)
	{
		alert("Please pick exactly 4 best suggestions");
		return false;
	}
	if (worst4.length != 4)
	{
		alert("Please pick exactly 4 worst suggestions");
		return false;
	}
	
	var best4names = $.makeArray(best4.map(function(){return $(this).attr("id")})).join("^");	
	var worst4names = $.makeArray(worst4.map(function(){return $(this).attr("id")})).join("^");
	var stop = Date.now();
	var elapsed = (stop-start);
		
	$.post("process.php", {workerId: userId, pid: pid, best:best4names, worst:worst4names, screenWidth: screen.width, screenHeight: screen.height, colorDepth: screen.colorDepth, timeElapsed:elapsed, rep:isRep}, function(data) {
		//reload the page
		window.location.reload(true);
	});
}

function onFinish()
{
	//check if the user typed a number
	if ($("#valid_field").val()=="")
	{
		alert("Please enter the number shown in the image, or type 'none' if there is no number");
		return false;
	}

	var good = false;
	var num = parseInt($("#valid_field").val().trim());
	//finish the experiment
	$.ajax({
	  type: 'POST',
	  async: false,
	  url: "finish.php",
	  data: {workerId: userId, number:num},
	  success: function(data) { good = true;}
	});
	return good;
}

function updateProgress()
{
	$("#progress").text(curState+1+" out of "+numStates);
}



</script>



</head>

<body id="main" onload="getState()">

<div id="interface">
	<span id="progress"></span>

	<h3> Instructions:</h3>
	<p>Imagine you want to color in this pattern. Please select the <b>4 colorings you like the most</b> and the <b>4 colorings you like the least</b> from the suggestions below. 
	You can select colorings by dragging them from the grid and into the respective "Favorite 4" or "Least Favorite 4" box.</p>
	<p>There are 6 pages total and one exit question. You may see the same pattern twice. </p>

	<div id="previewnotice"></div>

	<div id="display">
		<div id="suggestionGrid">
			Suggestions:
			<div id="grid"></div>
		</div>
	</div>
	
	<div id="answers">
		Favorite 4: <div id="best4" class="answerbox selected"></div>
		<br />
		Least Favorite 4: <div id="worst4" class="answerbox selected"></div>
	</div>
	<div id="nextarea">
		<form>			
			<input type="button" value="Next" name="next" id="next_button" />
		</form>
	</div>

</div>
<div id="thankyou" >
	<!-- https://www.mturk.com/mturk/externalSubmit -->
			<!-- https://workersandbox.mturk.com/mturk/externalSubmit; use https? -->
	Thank you for your participation! Please enter the number you see in the image below, or type "None" if there is no number.
	<!--https://www.mturk.com/mturk/externalSubmit-->
	<form id="answerform" action="thankyou.php" method="POST" onsubmit="return onFinish()" > 
			<img src="plate2.png" />
			<br />
			<input type="text" name="validation" id="valid_field" value="" />
			<input type="submit" name="send" id="finish_button" value="Finish" />
			<input type="hidden" name="screenWidth" id="sw_field" value="-1" />
			<input type="hidden" name="screenHeight" id="sh_field" value="-1" />
			<input type="hidden" name="colorDepth" id="cd_field" value="-1" />
			<input type="hidden" name="userAgent" id="ua_field" value="-1" />
			<input type="hidden" name="numColors" id="nc_field" value="0" />
	 
			<input type="hidden" name="assignmentId" id="myAssignmentId" value="" />
	</form> 
</div>

<div id="sorry">
	<h3> Sorry, you have already participated in a similar experiment. </h3>
</div>










</body>
</html>
