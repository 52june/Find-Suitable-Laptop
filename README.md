# Find-Suitable-Laptop
'가성비' 좋은 노트북 탐색 분석 (2017.10 ~ 2017.11)

시중에 나와있는 노트북 제품들의 가격과 구성요소 데이터를 이용해 대학생을 대상으로 *가성비*가 좋은 노트북을 찾기 위한 모델링을 실시 \
('가성비' : 노트북의 기능으로 예측한 값과 실제 값의 차이)

정확한 가성비를 계산하기 위해서는 가격을 정확하게 예측하는 것이 중요하기 때문에 다양한 Machine Learning 기기법들을 사용 \
(Linear Regression, Random Forest, Generalized Additive Model (GAM), Support Vector Machine (SVM), Boosting 등 - 최종 모형 RMSE = 0.10)

분석 이후 활용 방안으로
- 사용자가 선호하는 기능에 맞게 노트북을 추천해주는 알고리즘
- 임의로 정한 성능 요소들을 모형에 적용시켜 사용자 맞춤형 노트북의 적정 가격대를 예측하는 알고리즘
을 제작
