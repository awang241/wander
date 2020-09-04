<template>
  <div class="card" :style="cardStyle">
    <p>{{notification.message}}</p>
  </div>
</template>

<script>
import toastMixin from "../mixins/toastMixin";
import store from "../store";

export default {
  name: "Notification",
  mixins: [toastMixin],
  data() {
    return {
      store: store,
      cardStyle: {
        backgroundColor: null
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
      if (this.$props.notification.notificationType >= 0 && this.$props.notification.notificationType < 3) {
        this.cardStyle.backgroundColor = "#a6b5ff";
      } else if (this.$props.notification.notificationType >= 3 && this.$props.notification.notificationType < 9) {
        this.cardStyle.backgroundColor = "#ffb0b0";
      } else if (this.$props.notification.notificationType >= 9 && this.$props.notification.notificationType < 15) {
        this.cardStyle.backgroundColor = "#99ff94";
      } else {
        this.cardStyle.backgroundColor = "#ffffff"
      }
  }
}
</script>

<style scoped>
  .card{
    padding: 1rem;
  }

</style>

