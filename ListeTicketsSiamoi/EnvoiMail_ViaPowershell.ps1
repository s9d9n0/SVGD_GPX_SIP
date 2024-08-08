
#$from = "xxx.xxx@xxx.fr"
$from = "centreon-SIP@xxx.fr"
$to = "xxx.xxx@xxx.fr"

#$user = "idep@ad.xxx.intra"
#$secpasswd = ConvertTo-SecureString "xxx" -AsPlainText -Force

$user = "AD/PD0-SUP-SVC"

$secpasswd = ConvertTo-SecureString '8*wAjIGoa@uGsd@ASOhF' -AsPlainText -Force

$mycreds = New-Object System.Management.Automation.PSCredential($user, $secpasswd)

$smtp = "smtp.appli.xxx.fr"
$sujet = "Resume sur les tickets du GPX"
   
$pj = "C:\Users\SIAR_ycg8l6\Docs\ProgrammesR\Liste_TicketsSiamoi_Gepex\Fichiers_date_du_jour\Retranscription_Monitoring.zip"  

$message = Get-Content -Path "C:\Users\SIAR_ycg8l6\Docs\ProgrammesR\Liste_TicketsSiamoi_Gepex\message.txt" -Raw

Send-MailMessage -To $to -From $from -Subject $sujet -Attachments $pj -Body $message -Credential $mycreds -port 587 -smtp $smtp -UseSsl