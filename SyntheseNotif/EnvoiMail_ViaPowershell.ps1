
$from = "xxx.xxx@xxx.fr"
$to = "xxx.xxx@xxx.fr"

$user = "idep6@ad.xxx.intra"
#$secpasswd = ConvertTo-SecureString "xxx" -AsPlainText -Force

$mycreds = New-Object System.Management.Automation.PSCredential($user, $secpasswd)

$smtp = "smtp.appli.xxx.fr"
$sujet = "Resume Notifications"
$pj1 = "C:\Users\SIAR_ycg8l6\Docs\ProgrammesR\Centreon_SyntheseNotifCentreon\General\ResumNotif_24dernheures.ods"
$pj2 = "C:\Users\SIAR_ycg8l6\Docs\ProgrammesR\Centreon_SyntheseNotifCentreon\General\Liste_Notifications.html"

$html = Get-Content -Path "C:\Users\SIAR_ycg8l6\Docs\ProgrammesR\Centreon_SyntheseNotifCentreon\General\CorpsMel_Notifications.html" -Raw

Send-MailMessage -To $to -From $from -Subject $sujet -Attachments $pj1,$pj2 -Body $html -BodyAsHTML -Credential $mycreds -port 587 -smtp $smtp -UseSsl