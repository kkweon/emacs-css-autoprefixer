;;; css-autoprefixer-test.el --- Test for css-autoprefixer.el

;;; Code:
(load-file "css-autoprefixer.el")

(ert-deftest autoprefixer--execute-npx
    ()
  "Test if the autoprefxier works"
  (let ((temp-file "temp.css"))
    (with-temp-file temp-file
      (insert "a {
  display: flex;
}"))
    (let* ((result (css-autoprefixer--execute-npx temp-file))
           (code (car result))
           (content (car (cdr result))))
      (should (equal 0 code))
      (should (equal "a {
  display: -webkit-box;
  display: -ms-flexbox;
  display: flex;
}" (css-autoprefixer--trim-first-and-last content))))
    (delete-file temp-file)))

(ert-deftest autoprefixer--test-fail
    ()
  "When the autoprefixer fail, nothing should change"
  (with-temp-buffer
    (insert "this is wrong css syntax so it wil fail")
    (css-autoprefixer)
    (should (equal (buffer-string) "this is wrong css syntax so it wil fail"))))

;;; css-autoprefixer-test.el ends here
