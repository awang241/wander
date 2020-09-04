<template>
  <div class="card" :style="cardStyle">
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
      store: store,
      cardStyle: {
        backgroundColor: "#ffffff"
      }
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
      if (this.notificationData.type >= 0 && this.notificationData.type <3){
        this.cardStyle.backgroundColor == "#a6b5ff";
      }
      if (this.notificationData.type >= 3 && this.notificationData.type <9){
        this.cardStyle.backgroundColor == "#ffb0b0";
      }
      if (this.notificationData.type >= 9 && this.notificationData.type <15){
        this.cardStyle.backgroundColor == "#99ff94";
      }

  }
}
</script>

<style scoped>
  .card{
    padding: 1rem;
  }

</style>

