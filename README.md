# istanbul_traffic_data_visualization
İBB OPEN DATA PORTAL’dan 2021 Mart Ayı Trafik Yoğunluk veri setinin R programı ile görselleştirmesi üzerine bir çalışma.

İşbu veri kaynağının içerisinde, İstanbul'un coğrafi olarak eşit parçalara bölününce bu parçalarda bulunan tekil araçların sayısı, 
bu araçlara ait ortalama hız, maksimum hız ve minimum hızları bulunmaktadır.

Data Dictionary

# DATE_TIME : Tarih ve Saat bilgisini içeren alandır. Veri formati YYYY-MM-DD HH24:MI:SS formatındadır. * Tarih kırılımı saatliktir.
# LONGITUDE : Boylam bilgisini içeren alandır.
# LATITUDE : Enlem bilgisini içeren alandır.
# GEOHASH : Enlem ve Boylamların Geohash Değeridir, Geohash uzunluğu 6’dır.
# MINIMUM_SPEED : Verilen saatte ilgili geohash alanı için asgari hız (km/s cinsinden).
# MAXIMUM_SPEED : Verilen saatte ilgili geohash alanı için azami hız (km/s cinsinden).
# AVERAGE_SPEED : Verilen saatte ilgili geohash alanı için ortalama hız (km/s cinsinden).
# NUMBER_OF_VEHICLES : Verilen saatte ilgili geohash alanı içinde bulunan farklı araç sayısı.
