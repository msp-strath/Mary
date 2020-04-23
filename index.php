<?php

$site_root = "../MarySites/";

if (filter_has_var(INPUT_GET, "page")) {

  $user_id = escapeshellarg($_SERVER['HTTP_CIS_REMOTE_USER']);
  $page_id = escapeshellarg($_GET["page"]);


  $descriptorspec = array(
     0 => array("pipe", "r"),  // stdin is a pipe that the child will read from
     1 => array("pipe", "w"),  // stdout is a pipe that the child will write to
     2 => array("file", "/tmp/error-output.txt", "a") // stderr is a file to write to
  );

  if (empty($user_id)) { $userarg = ""; } else { $userarg = "--user=$user_id"; }

  $cmd = "./mary find $userarg $site_root $page_id | ./pandoc --data-dir=data --standalone -f markdown --filter=marypandoc.sh -t html --template templates/mary.html5 2>&1";

  $cwd = NULL;
  $env = NULL;

  $process = proc_open($cmd, $descriptorspec, $pipes, $cwd, $env);

  if (is_resource($process)) {
      // $pipes now looks like this:
      // 0 => writeable handle connected to child stdin
      // 1 => readable handle connected to child stdout
      // Any error output will be appended to /tmp/error-output.txt

      fwrite($pipes[0], serialize($_POST));
      fwrite($pipes[0], serialize($_GET));
      fclose($pipes[0]);

      echo stream_get_contents($pipes[1]);
      fclose($pipes[1]);

      // It is important that you close any pipes before calling
      // proc_close in order to avoid a deadlock
      $return_value = proc_close($process);
  }
}
elseif (filter_has_var(INPUT_GET, "pub")) {
  $pub_id = filter_input(INPUT_GET, 'pub', FILTER_SANITIZE_SPECIAL_CHARS);
  $target = realpath($site_root . "/" . $pub_id);
  if (!$target) {
    // realpath returns false if the target does not exist.
    trigger_error("Specified file does not exist.", E_USER_ERROR);
  }
  elseif (strpos($target, "/pub/") === FALSE) {
    // The user asked for a file not in a pub directory; we cannot
    // allow this! We use the same error message here as above in
    // order to not leak information.
    trigger_error("Specified file does not exist.", E_USER_ERROR);
  }
  elseif (!(strpos($target, realpath($site_root)) === 0)) {
    // The user is trying to access a file outside the site root!
    trigger_error("Specified file does not exist.", E_USER_ERROR);
  }
  else {
      // We are good, ship the file!
      $mime_type = mime_content_type($target);
      header('Content-Type: '.$mime_type);
      readfile($target);
  }
}
else {
  trigger_error("No page or pub given", E_USER_ERROR);
}
?>
