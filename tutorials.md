---
layout: default
title:  "Tutorials"
section: "tutorials"
---
{% include_relative _tut/tutorials.md %}

{% for x in site.tut %}
{% if x.section == 'tutorials' %}
- [{{x.title}}]({{site.baseurl}}{{x.url}})
{% endif %}
{% endfor %}
