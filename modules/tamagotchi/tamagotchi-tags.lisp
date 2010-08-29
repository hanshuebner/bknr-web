(in-package :bknr-user)

(enable-interpol-syntax)

(define-bknr-tag tamagotchi (&key id)
  (let ((tamagotchi (find-store-object id :class 'tamagotchi :query-function #'tamagotchi-with-name)))
    (let* ((images (get-keywords-intersection-store-images
		    (list (tamagotchi-tama-type tamagotchi)
			  (tamagotchi-status tamagotchi))))
	   (image (first images)))
      (html ((:div :align "center")
	     ((:img :name "tamagotchi" :src (format nil "/image/~a/double,8"
						    (store-object-id image))))
	     ((:script :type "text/javascript")
	      (:princ #?"var pictures = new Array();
var preloadarr = new Array();
var j = 0;
var animationSpeed = $((- 5500 (* 500 (tamagotchi-stress tamagotchi))));
function preload() {
  var i = 0;
  for (i = 0; i < pictures.length; i++) {
    preloadarr[i] = new Image();
    preloadarr[i].src = pictures[i];
  }
}

function refresh() {
   window.location.href = window.location.href;
}
setTimeout('refresh()', 30 * 1000);   
function animate() {
   document.images.tamagotchi.src = preloadarr[j].src
   j = j + 1
   if (j > (pictures.length - 1)) j = 0
   t = setTimeout('animate()', animationSpeed)
}
")
	      (loop for image in images
		    for i from 0
		    do (html (:princ (format nil "pictures[~A] = '/image/~A/double,8';~%"
					     i
					     (store-object-id image)))))
	      (:princ "preload(); animate();"))
	
	     (:table (:tr (:td "stress")
			  (:td (:princ-safe (tamagotchi-stress tamagotchi))))
		     (:tr (:td "happiness")
			  (:td (:princ-safe (tamagotchi-happiness tamagotchi))))
		     (:tr (:td "hunger")
			  (:td (:princ-safe (tamagotchi-hunger tamagotchi))))
		     (:tr (:td "tired")
			  (:td (:princ-safe (tamagotchi-tired tamagotchi))))
		     (:tr (:td "sleeping")
			  (:td (:princ-safe (tamagotchi-sleeping tamagotchi))))
		     (:tr (:td "poop")
			  (:td (:princ-safe (tamagotchi-poop tamagotchi)))))
	     ((:form :method "POST")
	      (:p (submit-button "feed" "feed")
		  (submit-button "play" "play")
		  (submit-button "joint" "joint")
		  (submit-button "ecstasy" "ecstasy")
		  (submit-button "coffee" "coffee")
		  (submit-button "beer" "beer")
		  (submit-button "clean" "clean")
		  (submit-button "hurt" "hurt"))))))))
		    
