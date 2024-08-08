
#$from = "xxx.xxx@xxx.fr"
$to = "xxx.xxx@xxx.fr"

#$user = "idep@ad.xxx.intra"
#$secpasswd = ConvertTo-SecureString "xxx" -AsPlainText -Force

$user = "AD/PD0-xxx-SVC"
$secpasswd = ConvertTo-SecureString '8*wAjIGoa@uGsd@ASOhF' -AsPlainText -Force

$mycreds = New-Object System.Management.Automation.PSCredential($user, $secpasswd)

$smtp = "smtp.appli.xxx.fr"
$sujet = "Envoi des fichiers"
   
$pj1 = "C:\Users\SIAR_ycg8l6\Docs\ProgrammesR\Detection_DDoS\df_PaloAlto.csv"  
$pj2 = "C:\Users\SIAR_ycg8l6\Docs\ProgrammesR\Detection_DDoS\df_Dns.csv" 

$message = Get-Content -Path "C:\Users\SIAR_ycg8l6\Docs\ProgrammesR\Detection_DDoS\message.txt" -Raw

Send-MailMessage -To $to -From $from -Subject $sujet -Attachments $pj1,$pj2 -Body $message -Credential $mycreds -port 587 -smtp $smtp -UseSsl