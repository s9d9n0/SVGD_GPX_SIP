
$from = "xxx.xxx@xxx.fr"
$to = "xxx.xxx@xxx.fr"

$user = "idep@ad.xxx.intra"
$secpasswd = ConvertTo-SecureString "xxx" -AsPlainText -Force

#$user = "AD/PD0-xxx-SVC"
#$secpasswd = ConvertTo-SecureString "DRWvPGUGTx+KwPR" -AsPlainText -Force
#$secpasswd = ConvertTo-SecureString "WCs*5Zd@nlOrqz4" -AsPlainText -Force
#$secpasswd = ConvertTo-SecureString '4RFVcde\"2ZSXwqa&' -AsPlainText -Force
#$secpasswd = ConvertTo-SecureString '9SqV*PZ6cKYS#QHx0t#Q' -AsPlainText -Force
#$secpasswd = ConvertTo-SecureString 'dzW^Wdq6O6LqeE+pcVrr' -AsPlainText -Force
#$secpasswd = ConvertTo-SecureString '3ln6qD@haqwuGIiA*AW3' -AsPlainText -Force

$mycreds = New-Object System.Management.Automation.PSCredential($user, $secpasswd)

$smtp = "smtp.appli.xxx.fr"
$sujet = "RESUME Evenements"

$pj1 = "C:\Users\SIAR_ycg8l6\Docs\ProgrammesR\CompteurAlertes_5min_DetectionRupture\resum_jour.csv"
$pj2 = "C:\Users\SIAR_ycg8l6\Docs\ProgrammesR\CompteurAlertes_5min_DetectionRupture\resum_jour_retourOK_debutKO.csv"
       
$message = Get-Content -Path "C:\Users\SIAR_ycg8l6\Docs\ProgrammesR\CompteurAlertes_5min_DetectionRupture\message_DetectRupt.txt" -Raw

Send-MailMessage -To $to -From $from -Subject $sujet -Attachments $pj1,$pj2 -Body $message -Credential $mycreds -port 587 -smtp $smtp -UseSsl