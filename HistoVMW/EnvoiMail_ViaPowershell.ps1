
$from = "xxx.xxx@xxx.fr"
#$from = "centreon-SIP@xxx.fr"
$to = "xxx.xxx@xxx.fr"

$user = "idep@ad.xxx.intra"
$secpasswd = ConvertTo-SecureString "xxx" -AsPlainText -Force

#$user = "AD/PD0-SUPERVISION-SVC"
#$secpasswd = ConvertTo-SecureString '3ln6qD@haqwuGIiA*AW3' -AsPlainText -Force

$mycreds = New-Object System.Management.Automation.PSCredential($user, $secpasswd)

$smtp = "smtp.appli.xxx.fr"
$sujet = "transfert du zip EsxiVM"
       
$message = Get-Content -Path "C:\Users\SIAR_ycg8l6\Docs\ProgrammesR\HistoVMW_EsxListeHost\message.txt" -Raw

Send-MailMessage -To $to -From $from -Subject $sujet -Body $message -Credential $mycreds -port 587 -smtp $smtp -UseSsl