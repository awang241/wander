<template>
  <div class="card">
    <p>{{notification.text}}</p>
  </div>
</template>

<script>
import toastMixin from "@/mixins/toastMixin";
import store from "@/store";

export default {
  name: "Notification",
  mixins: [toastMixin],
  data() {
    return {
      NotificationData: {},
      store: store
    }
  },
  props: {
    notification: {
      type: Object,
      required: true
    }
  },
  mounted() {
    if (this.props != undefined) {
      this.notificationData = this.props.notification;
    }

  }
}
</script>

<style scoped>
  .card{
    padding: 1rem;
  }

</style>