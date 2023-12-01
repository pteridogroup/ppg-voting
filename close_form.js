function closeForm() {
  // Close form
  const form = FormApp.getActiveForm();
  form.setAcceptingResponses(false);
  // Format current date in UTC+0
  var now = new Date();
  var formattedNow = Utilities.formatDate(now, "GMT", "yyyy-MM-dd'T'HH:mm:ss'GMT'");
  // Compose email
  const formName = form.getTitle();
  const url = form.getEditUrl() + "#responses";
  const recipient = "joelnitta@gmail.com";
  const subject = formName + " is now closed";
  const body = formName + " is now closed as of " + formattedNow + ".\nPlease check the following URL: " + url;
  // Send email
  GmailApp.sendEmail(recipient, subject, body) 
}