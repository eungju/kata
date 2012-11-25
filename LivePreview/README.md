# LivePreview: watchr + Chrome LivePage Extension

마크업 언어로 문서를 작성할 때 HTML로 변환한 결과를 바로 확인하고 싶을 때가 많다. Mou, Marked 등 이런 문제를 해결해주는 도구가 있지만 대부분 맥에서만 쓸 수 있고, 문서 처리 과정을 완전히 통제하지 못하는 등 다른 제약들도 있다. watchr과 크롬 LivePage를 이용하면 이 문제를 간편하게 해결할 수 있어서 소개한다.

## watchr

[watchr](https://github.com/mynyml/watchr)은 특정 파일이 변경됐을 때 어떤 동작을 취하도록 프로그램할 수 있다. 주로 Continous Testing 도구로 사용되지만, 파일 변경에 따른 처리 결과를 빠르게 알고 싶은 모든 경우에 아주 유용하다. 루비 gem으로 간단하게 설치할 수 있다.

```
$ gem install watchr
```

확장자가 md인 마크다운 문서가 변경되면 pandoc을 이용해서 HTML로 변환하는 watchr 스크립트를 다음과 같이 작성한다. 마크다운 처리기마다 문법이 조금씩 다른데 원하는 처리기가 실행되도록 알맞게 고친다.

```ruby
watch('(.*)\.md') do |m|
  spawn("pandoc -f markdown -t html5 -o #{m[1]}.html #{m[0]}")
end
```

그리고 다음과 같이 위의 스크립트를 watchr로 실행하면 마크다운 문서를 수정하면 pandoc이 HTML 문서로 변환한다.

```
$ watchr livepreview.rb
```

브라우저에서 HTML 문서를 열어서 변환 결과를 확인할 수 있다. 하지만 매번 새로 고침(refresh)을 해야하기 때문에 불편하다. 이 때 크롬의 LivePage 확장으로 이 문제를 해결할 수 있다.

## LivePage Extension

크롬의 [LivePage Extension](https://chrome.google.com/webstore/detail/livepage/pilnojpmdoofaelbinaeodfpjheijkbh) 확장은 웹 리소스가 변경되면 감지해서 바로 반영해준다. 로컬 HTML도 변경되면 바로 반영해주기 때문에 새로 고침을 하지 않아도 변경된 결과를 즉시 볼 수 있다. 크롬에서 LivePage 확장을 설치하고 <chrome://chrome/extensions/> 에서 LivePage의 "Allow access to file URLs"를 체크하여 로컬 파일에도 접근할 수 있게 해준다.

HTML 파일을 크롬에서 열고 LivePage 버튼을 눌러서 "LivePage"를 활성화한다. 이제 마크다운 문서를 변경하고 저장하면 watchr이 이를 감지해서 HTML로 변환하고 HTML의 변경을 LivePage가 감지해서 크롬에서 보여준다. 즉 편집기에서 저장하면 결과를 바로 브라우저에서 확인할 수 있다.
