## Bank Application

## Requerimientos

Para construir esta aplicacion ud necesita:

- [JDK 11](https://jdk.java.net/11/)
- [Maven 3](https://maven.apache.org)
- [PostgreSql >=10.5](https://www.postgresql.org) o [Docker](https://www.docker.com/)

## Correr la aplicacion localmente

<ol>
<li>
<h4>Para desplegar la aplicación en local haciendo uso de Docker  es necesario contar con :</h4>
- Docker<br> 
- Docker Compose
<br>
Desde una terminal ubicada en la raiz del proyecto ejecutar:
<br><code>docker build -t postgres-bankinc:latest ./docker/postgres ; mvn compile jib:dockerBuild ; docker-compose -f docker/docker-compose.yml up</code>
</li>
<li>Tambien puede utilizar Spring Boot Maven plugin:
  <br><code>mvn spring-boot:run -Dspring-boot.run.profiles=dev</code>
</li>
<li>
Para ejecutar la aplicacion Spring Boot en tu maquina local  es necesario ejecutar el método <code>main</code> en la clase 
<code>com.trunks.springbootbankinc.SpringApplication</code> desde su IDE preferido :)
</li>
</ol>