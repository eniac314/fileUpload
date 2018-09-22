<?php
include 'utils.php';
if(getenv('REQUEST_METHOD') == 'POST' && !empty($_FILES['file']) && $_FILES['file']['error'] == 0  && isset($_POST['sessionId']) && isset($_POST['formToken'])) {
    
  session_id($_POST["sessionId"]);
  session_start();

  if (!isset($_SESSION['logInfo']['username']) || !isset($_SESSION['logInfo']['formToken']) || ($_POST["formToken"] !== $_SESSION['logInfo']['formToken'])){
      logError("wrong credentials");
      exit();
  }

  
  if(filesize($_FILES['file']['tmp_name']) > 1000000000){
    logError("file is too big: ".filesize($_FILES['file']['tmp_name']));  
    exit();
  } 
 
  $db = mysqli_connect($mysql_server, $mysql_user, $mysql_password, $mysql_db);

	if (mysqli_connect_errno()){
	  logError('Could not connect to database');
	  exit();
	}
    
  $stmt  = mysqli_stmt_init($db);
  
  $query = "SELECT usrType FROM users WHERE name = ?";
  mysqli_stmt_prepare($stmt, $query);
  mysqli_stmt_bind_param($stmt,'s', $_SESSION['logInfo']['username']);
  mysqli_stmt_execute($stmt);
  mysqli_stmt_bind_result($stmt, $usrType);

  if (!mysqli_stmt_fetch($stmt)){
    logError("could not verify user");
    mysqli_close($db);
    exit();
  }

  if (($usrType != 1)){
    logError("user is not an admin");
    mysqli_close($db);
    exit();
  }

  $getUUIDQuery = "SELECT REPLACE(UUID(),'-','')";
  mysqli_stmt_prepare($stmt, $getUUIDQuery);
  mysqli_stmt_execute($stmt);
  mysqli_stmt_bind_result($stmt, $uuid);

  if (!mysqli_stmt_fetch($stmt)){
     logError("could not get UUID");
     mysqli_close($db);
     exit();
  }

  $uploaddir = './content/files';

  /*check if destination directory exists, if not creates it*/
  if (!file_exists($uploaddir) && !is_dir($uploaddir)) {
    mkdir($uploaddir);
  } 
  
  $newfilename = uuidnam_sfx($uploaddir, $uuid,".tmp");

  if (!move_uploaded_file($_FILES['file']['tmp_name'], $newfilename)) {
	  logError("File upload failed");
	  exit();
	} 
  
  $query = "INSERT INTO files(tmpname, filename) VALUES (?,?)";
      
  mysqli_stmt_prepare($stmt, $query);
  mysqli_stmt_bind_param($stmt,'ss', $newfilename, $_FILES['file']['name']);
  $res = mysqli_stmt_execute($stmt);
  
  if (mysqli_stmt_affected_rows($stmt) == 0 || !$res){
    logError("Data was not inserted into database ".mysqli_error($db).mysqli_stmt_error($stmt));
   
    $info = pathinfo($newfilename);

    //delete original picture
    if(file_exists($newfilename)){
      if (!unlink($newfilename)){
        logError("Could not delete the file");
        mysqli_close($db);
        exit();
      }
    }
  }
 

  echo "all is good";
  mysqli_close($db);
  exit();
}
logError("invalid request");
exit();
?>