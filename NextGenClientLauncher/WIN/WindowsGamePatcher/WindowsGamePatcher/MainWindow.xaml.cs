    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Threading.Tasks;
    using System.Windows;
    using System.Windows.Controls;
    using System.Windows.Data;
    using System.Windows.Documents;
    using System.Windows.Input;
    using System.Windows.Media;
    using System.Windows.Media.Imaging;
    using System.Windows.Navigation;
    using System.Windows.Shapes;
    using System.IO;
    using Ionic.Zip;
    using System.Net;
    using System.Diagnostics;
    using System.Xml.Serialization;
    using System.Xml;

    namespace WindowsGamePatcher
    {
        /// <summary>
        /// Interaction logic for MainWindow.xaml
        /// </summary>
        public partial class MainWindow : Window
        {
            string zipPath;// = @"C:\ProgramFiles\Game\Game.zip";
            string gamePath;// = @"C:\ProgramFiles\Game";
            string currentVersionFile;
            WebClient wc = new WebClient();

            public MainWindow()
            {

                SetUpDirectories();
                InitializeComponent();

                if (!CheckConnection("https://www.google.ie/"))
                {
                    MessageBox.Show("Cannot Connect, Please check your Connection");
                    Application.Current.Shutdown();
                }
                    

                btn_Launch.IsEnabled = false;
                DownloadCurrentGameVersion();
            }

            private void SetUpDirectories()
            {
                zipPath = Directory.GetCurrentDirectory() + "\\Game.zip";
                gamePath = Directory.GetCurrentDirectory();

                if (!Directory.Exists(gamePath))
                    Directory.CreateDirectory(gamePath);

                currentVersionFile = gamePath + "\\ServerTest\\currentVersion.xml";
            }

            private void btn_Launch_Click(object sender, RoutedEventArgs e)
            {
                Process.Start(gamePath + "\\ServerTest\\Abydos Record Test.exe");
            }

            void wc_DownloadFileCompleted(object sender, System.ComponentModel.AsyncCompletedEventArgs e)
            {
                txt_Status.Text = "State : Ready";
                using (ZipFile zip1 = ZipFile.Read(zipPath))
                {
                    foreach (ZipEntry zip in zip1)
                    {
                        zip.Extract(gamePath, ExtractExistingFileAction.OverwriteSilently);
                    }
                }
                File.Delete(currentVersionFile);
                File.Move(gamePath + "\\version.xml", currentVersionFile);

                btn_Launch.IsEnabled = true;
            }

            void wc_DownloadFileCompletedXML(object sender, System.ComponentModel.AsyncCompletedEventArgs e)
            {
                wc.DownloadProgressChanged -= wc_DownloadProgressChangedXML;
                wc.DownloadFileCompleted -= wc_DownloadFileCompletedXML;

                XmlSerializer serializer = new XmlSerializer(typeof(Version));
                FileStream fStream;

                fStream = new FileStream(gamePath + "\\version.xml", FileMode.Open);
                XmlReader reader = XmlReader.Create(fStream);
                Version currentVersion = (Version)serializer.Deserialize(reader);
                fStream.Close();
                reader.Close();

                if (!File.Exists(currentVersionFile))
                {
                    StartGameDownload();
                    return;
                }
                
                fStream = new FileStream(currentVersionFile, FileMode.Open);
                reader = XmlReader.Create(fStream);
                Version serverVersion = (Version)serializer.Deserialize(reader);
                fStream.Close();
                reader.Close();

                if (currentVersion.versionNumber != serverVersion.versionNumber)
                    StartGameDownload();

                else
                {
                    txt_Status.Text = "State : Ready";
                    btn_Launch.IsEnabled = true;
                }
                     
                    
            }

            void wc_DownloadProgressChanged(object sender, DownloadProgressChangedEventArgs e)
            {
                txt_Status.Text = "State : Downloading";
                prgs_Download.Value = e.ProgressPercentage;
            }

            void wc_DownloadProgressChangedXML(object sender, DownloadProgressChangedEventArgs e)
            {
                txt_Status.Text = "State : Checking Version";
                prgs_Download.Value = e.ProgressPercentage;
            }

            private void StartGameDownload()
            {
                wc.DownloadProgressChanged += wc_DownloadProgressChanged;
                wc.DownloadFileCompleted += wc_DownloadFileCompleted;
                wc.DownloadFileAsync(new Uri("http://abydosentertainment.com/WorldRecordClient"), zipPath);
            }

            private void DownloadCurrentGameVersion()
            {
                wc.DownloadProgressChanged += wc_DownloadProgressChangedXML;
                wc.DownloadFileCompleted += wc_DownloadFileCompletedXML;
                wc.DownloadFileAsync(new Uri("http://abydosentertainment.com/WorldRecordClientXML"), gamePath + "\\version.xml");
            }

            private bool CheckConnection(String URL)
            {
                try
                {
                    HttpWebRequest request = (HttpWebRequest)WebRequest.Create(URL);
                    request.Timeout = 5000;
                    request.Credentials = CredentialCache.DefaultNetworkCredentials;
                    HttpWebResponse response = (HttpWebResponse)request.GetResponse();

                    if (response.StatusCode == HttpStatusCode.OK) return true;
                    else return false;
                }
                catch
                {
                    return false;
                }
            }


        }

        [Serializable]
        public struct Version
        {
            public float versionNumber;
        }
    }
