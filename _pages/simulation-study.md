---
title: "Simulation Study"
permalink: /simulation-study/
header:
  image: "/images/projects1.jpg"
---


<ul>
  {% for post in site.posts %}
    <li>
      <a href="{{ post.permalink }}">{{ post.title }}</a>
      <p>{{ post.excerpt }}</p>
    </li>
  {% endfor %}
</ul>