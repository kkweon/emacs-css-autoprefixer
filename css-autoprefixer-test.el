;;; css-autoprefixer-test.el --- Test for css-autoprefixer.el

;;; Code:
(load-file "css-autoprefixer.el")

(ert-deftest autoprefixer-remove-first-and-last-line
    ()
  (should (equal "middleline" (css-autoprefixer--trim-first-and-last "first\nmiddleline\nlastline"))))

(ert-deftest autoprefixer--build-npx-command
    ()
  (let ((temp-file "temp.css"))
    (with-temp-file temp-file
      (insert "a {
  display: flex;
}"))
    (should (equal "a {
  display: -webkit-box;
  display: -ms-flexbox;
  display: flex;
}" (css-autoprefixer--execute-npx temp-file)))
    (delete-file temp-file)))


;;; css-autoprefixer-test.el ends here
